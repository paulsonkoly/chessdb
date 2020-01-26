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
  foreign_key :game_id, :games, type: 'bigint', null: false, on_delete: :cascade
  String :fen_position, size: 72, null: false
  String :san, size: 10, null: false
  String :next_san, size: 10
  Fixnum :active_colour, type: 'bit', null: false
  Fixnum :fullmove_number, type: 'smallint', null: false
  Fixnum :castling_availability, type: 'smallint', null: false
  Fixnum :halfmove_clock, type: 'smallint', null: false
  Fixnum :en_passant, type: 'smallint'

  index :castling_availability, name: 'ix_moves_on_castling_availability',
                                type: 'btree'
  index :en_passant, name: 'ix_moves_on_en_passant', type: 'btree'
  index :fen_position, name: 'ix_moves_on_fen_position', type: 'btree'
  index :fen_position, name: 'ix_moves_on_fen_position_hash', type: 'hash'
  index :fullmove_number, name: 'ix_moves_on_fullmove_number', type: 'btree'
  index :game_id, name: 'ix_moves_on_game_id', type: 'btree'
  index :active_colour, name: 'ix_moves_on_active_colour', type: 'btree'
end

run 'CLUSTER "moves" USING "ix_moves_on_fen_position"'
