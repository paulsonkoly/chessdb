require 'roda'
require 'yaml'
require 'sequel'
require 'hiredis'

class Cache
  def initialize
    @redis = Hiredis::Connection.new
    @redis.connect('localhost', 6379)
    @mutex = Mutex.new
  end

  def fetch(key, &block)
    response = read(key)
    if response.nil?
      value = yield
      write(key, value)
      value
    else
      response
    end
  end

  private

  def read(key)
    value = cache_action { @redis.write ['GET', key] }
    Marshal.load(value) unless value.nil?
  end

  def write(key, value)
    value = Marshal.dump(value)
    cache_action { @redis.write ['SET', key, value] }
  end

  def cache_action
    @mutex.synchronize do
      yield
      @redis.read
    end
  end
end

AppCache = Cache.new

## DATABASE
database_config_file = File.join(File.dirname(__FILE__), 'config', 'database.yml').freeze
config_yaml = File.read(database_config_file)
database_connection_params = YAML.load(config_yaml, symbolize_names: true)

DB = Sequel.connect(
  adapter: 'postgres',
  **database_connection_params[:development])

class App < Roda
  plugin :assets,
    css: ['application.scss', 'svg-with-js.min.css'],
    js: ['application.js', 'chessboard.js', 'jquery-min.js']
  plugin :public
  opts[:root] = File.expand_path(__dir__)
  plugin :static ['/public']

  route do |r|
    r.assets
    r.public

    r.on 'games' do
      r.get Integer do |id|
        <<~EOHTML
          <!DOCTYPE html>
          <html>
            <head>
              <title>Chessdb</title>
              <meta charset="UTF-8">
              #{assets(:css)}
              #{assets(:js)}
            </head>

            <body>
              <div class="grid-conatiner">
                <div class="grid-x grid-padding-x grid-padding-y">
                  <div class="cell">
                    <div class="grid-x">
                      <div class="cell small-3">
                        <h5>
                          Morovic Fernandez, Ivan<span class="elo">(2535)</span> - Korchnoi, Viktor<span class="elo">(2615)</span>
                        </h5>
                        <hr />
                        Santiago m, Santiago CHI, 
                      </div>
                      <div class="cell small-8"/>
                        <div id="game_viewer"></div>
                      </div>
                      <script>
                        var app = Elm.Main.init({
                          node: document.getElementById("game_viewer"),
                          flags: #{id}
                        });
                        var chessboard;

                        app.ports.signalDomRendered.subscribe(function (msg) {
                          requestAnimationFrame(function() {
                            chessboard = ChessBoard('chessboard', 'start');
                          });
                        });

                        app.ports.signalFenChanged.subscribe(function (fen) {
                          chessboard.position(fen);
                        });
                      </script>
                    </div>
                  </div>
                </div>
              </div>
            </body>
          </html>
        EOHTML
      end

      r.get(/(\d+).json/) do |id|
        columns = DB[:moves].columns.map do |column|
          case column 
          when :active_colour then Sequel.cast(:active_colour, Integer)
          else column
          end
        end
        ds = DB[:moves]
          .select(*columns)
          .where(game_id: id)
          .order(:fullmove_number)
          .order_append(:active_colour)

        response['Content-Type'] = 'application/json'
        { moves: ds.all }.to_json
      end
    end

    r.on 'moves' do
      r.get 'popularities' do
        token = r.params['token'].to_i
        fen = r.params['fen']

        response['Content-Type'] = 'application/json'

        data = AppCache.fetch(fen) do
          if fen == 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR'
            condition = { fullmove_number: 1 }
            next_san_column = :san
          else
            condition = { fen_position: fen }
            next_san_column = :next_san
          end

          ds = DB[:moves]
            .select(:result, Sequel.as(next_san_column, :next_san))
            .join(:games, [[:id, :game_id]])
            .exclude(next_san: nil)
            .where(condition)
            .from_self(alias: 'counts')
            .select(
              :next_san,
              Sequel.function(:count).*.filter(result: 0).as(:black_won),
              Sequel.function(:count).*.filter(result: 1).as(:white_won),
              Sequel.function(:count).*.filter(result: 2).as(:draw),

              (Sequel.function(:count).*.filter(result: 0) +
               Sequel.function(:count).*.filter(result: 1) +
               Sequel.function(:count).*.filter(result: 2)).as(:total_count))
            .group(:next_san)
            .order(Sequel.desc(:total_count))
          ds.all
        end

        {token: token, moves: data}.to_json
      end
    end
  end
end

begin
  run App.freeze.app
ensure
  # TODO: why???
#  $connection.close
end
