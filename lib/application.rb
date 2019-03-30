require 'autoprefixer-rails'

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
      { self.class.send(:application_scss) => Dir['assets/css/*.{scss,css}'] }
      .freeze
  end

  # :reek:ControlArgument
  def self.post_process_asset(_, type, content)
    case type
    when :css then AutoprefixerRails.process(content).css
    else content
    end
  end

  attr_reader :cache, :erb_store, :configuration, :repository, :dependency_map

  def self.application_scss
    full_path = File.join(Configuration::PROJECT_ROOT,
                          'assets',
                          'css',
                          'application.scss')
    File.expand_path(full_path)
  end
  private_class_method :application_scss
end
