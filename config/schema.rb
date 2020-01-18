create_table :games do
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
