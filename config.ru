require 'roda'

require_relative 'lib/application'

class App < Roda
  app = Application.new

  plugin :assets,
    css: ['application.scss', 'svg-with-js.min.css'],
    js: ['application.js', 'chessboard.js', 'jquery-min.js'],
    dependencies: app.dependency_map,
    postprocessor: Application.method(:post_process_asset)
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
        @active_menu = :none
        app.erb_store.resolve_html(:game_viewer, binding)
      end

      r.get(/(\d+).json/) do |id|
        { moves: @app.repository.moves_in_game(game_id: id).all }
      end

      r.get 'search' do
        @active_menu = :search
        app.erb_store.resolve_html(:game_search, binding)
      end

      r.post 'search' do
        app.repository.game_search(r.params)
      end
    end

    r.on 'moves' do
      r.get 'popularities' do
        token = r.params['token'].to_i
        fen = r.params['fen']

        data = AppCache.fetch(fen) do
          app.repository.popular_moves(fen: fen).all
        end

        { token: token, moves: data }
      end
    end
  end
end

run App.freeze.app
