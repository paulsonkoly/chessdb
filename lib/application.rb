require_relative 'cache'
require_relative 'erb_store'
require_relative 'repository'
require_relative 'configuration'

# Collects all parts of the application into an object
# :reek:TooManyInstanceVariables
class Application
  def initialize
    @cache = Cache.new
    @erb_store = ERBStore.new
    @erb_store <<
      { token: :game_viewer, filename: 'templates/game_viewer.html.erb' } <<
      { token: :game_search, filename: 'templates/game_search.html.erb' }.freeze
    @configuration = Configuration.new
    @repository = Repository.new(@configuration)
    @dependency_map =
      {
        File.join(Configuration::PROJECT_ROOT,
                  'assets',
                  'css',
                  'application.css') => Dir['assets/css/*.{scss,css}'] }.freeze
  end

  attr_reader :cache, :erb_store, :configuration, :repository, :dependency_map
end
