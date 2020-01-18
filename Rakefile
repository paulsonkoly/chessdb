require 'yaml'
require 'sequel'

def with_database(&block)
  db_config = YAML.load_file('config/chessdb.yml')['database'][:development]
  Sequel.connect(adapter: 'postgres', **db_config, &block)
end

namespace :db do
  desc 'Loads schema into the database'
  task :create do
    with_database do |db|
      db.create_table :games do
        primary_key :id, type: 'bigint'
        String :event, type: 'character varying'
        String :site, type: 'character varying'
        Date :date
        String :round, type: 'character varying'
        String :white, type: 'character varying'
        String :black, type: 'character varying'
        Fixnum :result, type: 'smallint'
        Fixnum :white_elo, type: 'smallint'
        Fixnum :black_elo, type: 'smallint'
        String :ECO, size: 10
        Date :event_date
      end
    end
  end

  desc 'Deletes tables'
  task :drop do
    with_database do |db|
      db.drop_table :games
    end
  end
end
