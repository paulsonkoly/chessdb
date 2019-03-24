require 'roda'
require 'yaml'
require 'pg'

## DATABASE
database_config_file = File.join(File.dirname(__FILE__), 'config', 'database.yml').freeze
config_yaml = File.read(database_config_file)
database_connection_params = YAML.load(config_yaml, symbolize_names: true)

$connection = PG.connect(database_connection_params[:development])

class App < Roda
  plugin :assets, css: 'application.scss', js: 'application.js'

  route do |r|
    r.assets

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
                      <div id="game_viewer" class="cell small-8"/></div>
                      <script>
                        var app = Elm.Main.init({
                          node: document.getElementById("game_viewer"),
                          flags: #{id}
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
        sql = <<~EOSQL
          select * from moves where moves.game_id = $1 order by fullmove_number, active_colour asc
        EOSQL
        pg_result = $connection.exec_params(sql, [id.to_i])
        moves_decoder = PG::TypeMapByColumn.new(
          [ PG::TextDecoder::Integer.new, # id
            nil,                          # game id
            PG::TextDecoder::String.new,  # fen_position
            PG::TextDecoder::String.new,  # san
            nil,                          # next_san
            PG::TextDecoder::Integer.new, # active_colour
            PG::TextDecoder::Integer.new, # fullmove_number
            PG::TextDecoder::Integer.new, # castling_availability
            PG::TextDecoder::Integer.new, # halfmove_clock
            PG::TextDecoder::Integer.new]) #en_passant
        pg_result.type_map = moves_decoder
        result = pg_result.ntuples.times.map { |ix| pg_result[ix] }

        response['Content-Type'] = 'application/json'
        { moves: result }.to_json
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
