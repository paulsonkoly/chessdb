require 'roda'
require 'yaml'
require 'pg'

## DATABASE
database_config_file = File.join(File.dirname(__FILE__), 'config', 'database.yml').freeze
config_yaml = File.read(database_config_file)
database_connection_params = YAML.load(config_yaml, symbolize_names: true)

$connection = PG.connect(database_connection_params[:development])


class App < Roda
  plugin :assets, css: 'application.scss'

  route do |r|
    r.assets

    r.on 'games' do
      r.get Integer do |id|
        "games triggered id: #{id}"
        assets_paths(:css).to_s
      end

      r.get(/(\d+).json/) do |id|
        sql = <<~EOSQL
          select * from moves where moves.game_id = $1 order by fullmove_number, active_colour asc
        EOSQL
        pg_result = $connection.exec_params(sql, [id.to_i])
        result = pg_result.ntuples.times.map { |ix| pg_result[ix] }
        response['Content-Type'] = 'application/json'
        result.to_json
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
