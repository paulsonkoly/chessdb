require 'roda'

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

run App.freeze.app
