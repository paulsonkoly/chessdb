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

create_table :moves do
  primary_key :id, type: 'bigint'
  Integer :game_id, type: 'bigint', null: false
  String :fen_position, size: 72, null: false
  String :san, size: 10, null: false
  String :next_san, size: 10
  Fixnum :active_colour, type: 'bit', null: false
  Fixnum :fullmove_number, type: 'smallint', null: false
  Fixnum :castling_availability, type: 'smallint', null: false
  Fixnum :halfmove_clock, type: 'smallint', null: false
  Fixnum :en_passant, type: 'smallint'
end
