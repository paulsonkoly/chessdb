require 'roda'

require_relative 'lib/cache'
require_relative 'lib/erb_store'
require_relative 'lib/repository'
require_relative 'lib/configuration'

AppCache = Cache.new
AppErb = ERBStore.new
AppErb <<
  { token: :game_viewer, filename: 'templates/game_viewer.html.erb' } <<
  { token: :game_search, filename: 'templates/game_search.html.erb' }
AppConfiguration = Configuration.new
AppRepository = Repository.new(AppConfiguration)

class App < Roda
  plugin :assets,
    css: ['application.scss', 'svg-with-js.min.css'],
    js: ['application.js', 'chessboard.js', 'jquery-min.js']
  plugin :public
  opts[:root] = Configuration::PROJECT_ROOT
  plugin :static ['/public']
  plugin :json
  plugin :json_parser, parser: -> str { JSON.parse(str, symbolize_names: true) }

  route do |r|
    r.assets
    r.public

    r.on 'games' do
      r.get Integer do |id|
        AppErb.resolve_html(:game_viewer, binding)
      end

      r.get(/(\d+).json/) do |id|
        { moves: AppRepository.moves_in_game(game_id: id).all }
      end

      r.get 'search' do
        AppErb.resolve_html(:game_search, binding)
      end

      r.post 'search' do
        AppRepository.game_search(r.params)
      end
    end

    r.on 'moves' do
      r.get 'popularities' do
        token = r.params['token'].to_i
        fen = r.params['fen']

        data = AppCache.fetch(fen) do
          AppRepository.popular_moves(fen: fen).all
        end

        { token: token, moves: data }
      end
    end
  end
end

run App.freeze.app
