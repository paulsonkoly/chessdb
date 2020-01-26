require 'yaml'
require 'sequel'

private

def with_database(&block)
  db_config = YAML.load_file('config/chessdb.yml')['database'][:development]
  Sequel.connect(adapter: 'postgres', **db_config, &block)
end

namespace :db do
  desc 'Loads schema into the database'
  task :create do
    with_database do |db|
      db.instance_eval(File.read('config/schema.rb'))
    end
  end

  desc 'Deletes tables'
  task :drop do
    with_database do |db|
      db.drop_table? :moves
      db.drop_table? :games
    end
  end
end
