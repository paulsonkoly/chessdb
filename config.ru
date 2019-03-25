require 'roda'
require 'yaml'
require 'sequel'

## DATABASE
database_config_file = File.join(File.dirname(__FILE__), 'config', 'database.yml').freeze
config_yaml = File.read(database_config_file)
database_connection_params = YAML.load(config_yaml, symbolize_names: true)

DB = Sequel.connect(
  adapter: 'postgres',
  **database_connection_params[:development])

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
  end
end

begin
  run App.freeze.app
ensure
  # TODO: why???
#  $connection.close
end
