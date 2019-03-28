require 'app_configuration'

# chessdb configuration based on config file + environment variables
class Configuration
  # The root directory of the project
  PROJECT_ROOT = File.join(File.dirname(__FILE__), '..').freeze

  # The config file path
  CONFIG_PATH = File.join(PROJECT_ROOT, 'config').freeze

  DEFAULT_APP_CONFIG = AppConfiguration.new do
    config_file_name 'chessdb.yml'
    base_local_path CONFIG_PATH
    base_global_path CONFIG_PATH
  end
  private_constant :DEFAULT_APP_CONFIG

  def initialize(config = DEFAULT_APP_CONFIG)
    @config = config
  end

  DEFAULT_CONFIG = self.new

  def database
    @config.database.transform_values do |h|
      h.merge!(password: database_password)
    end
  end

  def database_password
    @config.chessdb_database_password
  end
end
