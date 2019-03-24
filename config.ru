require 'roda'
require 'yaml'
require 'pg'

## DATABASE
database_config_file = File.join(File.dirname(__FILE__), 'config', 'database.yml').freeze
config_yaml = File.read(database_config_file)
database_connection_params = YAML.load(config_yaml, symbolize_names: true)

connection = PG.connect(database_connection_params[:development])

class App < Roda
  route do |r|
    r.on 'games' do
      r.get Integer do |id|
        "games triggered id: #{id}"
      end

      r.get(/(\d+).json/) do |id|
        "json handler triggered id: #{id}"
      end
    end
  end
end

begin
  run App.freeze.app
ensure
  connection.close
end
