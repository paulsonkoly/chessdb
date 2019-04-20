require 'yaml'
require 'sequel'
require 'logger'

# Repository for database access
class Repository
  def initialize(config)
    @db = Sequel.connect(adapter: 'postgres', **config.database[:development])
    @db.loggers = [Logger.new($stdout)]
  end

  def game(id:)
    @db[:games].where(id: id).first
  end

  def moves_in_game(game_id:)
    @db[:moves]
      .select(*game_columns)
      .where(game_id: game_id)
      .order(:fullmove_number)
      .order_append(:active_colour)
  end

  def popular_moves(fen:, castle:, active_colour:, en_passant:)
    Fen.for(fen, castle, active_colour, en_passant).popular_moves(@db)
  end

  def game_search(filter_hash)
    game_search_filter_paginated
      .run(@db[:games], filter_hash)
      .order_by(:id).limit(20).all
  end

  def game_count(filter_hash)
    game_search_filter
      .run(@db[:games], filter_hash)
      .select(Sequel.function(:count).*)
      .first[:count]
  end

  def position_search(filter_hash)
    position_search_filter_paginated
      .run(@db[:moves], filter_hash)
      .order_by(Sequel.qualify(:moves, :id)).limit(20)
      .join(:games, id: :game_id).all
  end

  def position_count(filter_hash)
    (position_search_filter)
      .run(@db[:moves], filter_hash)
      .select(Sequel.function(:count).*)
      .first[:count]
  end

  # Composable filter objects.
  class Filter
    def initialize(*keys, &block)
      @keys = keys
      @block = block
      @others = []
    end

    def <<(other)
      @others << other
      self
    end

    def +(other)
      self.dup.tap do |obj|
        obj.instance_variable_set(:@others, @others + [other])
      end
    end

    def run(table, hash)
      value = hash.dig(*@keys)
      table = @block.call(table, value) if value
      @others.each { |filter| table = filter.run(table, hash) }
      table
    end
  end

  private

  def game_search_filter
    @game_search_filter ||=
      (Filter.new(:white) { |table, actual| table.where(white: actual) }   <<
       Filter.new(:black) { |table, actual| table.where(black: actual) }   <<
       Filter.new(:either_colour) do |table, actual|
         table.where { (white =~ actual) | (black =~ actual) }
       end                                                                 <<
       Filter.new(:opponent) do |table, actual|
         table.where { (white =~ actual) | (black =~ actual) }
       end                                                                 <<
       Filter.new(:minimum_elo) do |table, actual|
         table.where { (white_elo >= actual) & (black_elo >= actual) }
       end                                                                 <<
       Filter.new(:maximum_elo) do |table, actual|
         table.where { (white_elo <= actual) & (black_elo <= actual) }
       end                                                                 <<
       Filter.new(:event) { |table, actual| table.where(event: actual) }   <<
       Filter.new(:site) { |table, actual| table.where(site: actual) }     <<
       Filter.new(:from_date) do |table, actual|
         table.where { date >= actual }
       end                                                                 <<
       Filter.new(:to_date) do |table, actual|
         table.where { date <= actual }
       end                                                                 <<
       Filter.new(:round) { |table, actual| table.where(round: actual) }   <<
       result_filter                                                       <<
       Filter.new(:eco) { |table, actual| table.where(eco: actual) }
      ).freeze
  end

  def game_search_filter_paginated
    @game_search_filter_paginated ||=
      pagination_filter + game_search_filter
  end

  def result_filter
    Filter.new(:result) do |table, actual|
      case actual
      when "0-1" then table.where(result: 0)
      when "1-0" then table.where(result: 1)
      when "Decisive" then table.where { result < 2 }
      when "1/2-1/2" then table.where(result: 2)
      else table
      end
    end
  end

  def pagination_filter
    @pagination_filter ||= Filter.new(:pagination, :offset) do |table, actual|
      table.offset(actual)
    end.freeze
  end


  def position_search_filter
    @position_search_filter ||=
      (Filter.new(:position, :active_colour) do |table, actual|
        table.where(active_colour: Sequel.cast(actual, :bit))
      end <<
      Filter.new(:position, :castling_availability) do |table, actual|
        table.where(castling_availability: actual)
      end <<
      Filter.new(:position, :fen_position) do |table, actual|
        table.where(fen_position: actual)
      end <<
      Filter.new(:position, :en_passant) do |table, actual|
        table.where(en_passant: actual)
      end
      ).freeze
  end

  def position_search_filter_paginated
    @position_search_filter_paginated ||=
      pagination_filter + position_search_filter
  end

  def game_columns
    @game_columns ||= @db[:moves].columns.map do |column|
      case column
      when :active_colour then Sequel.cast(:active_colour, Integer)
      else column
      end
    end
  end

  def self.count_columns
    %i[black_won white_won draw]
  end
  private_class_method :count_columns

  def self.counts
    count_columns.map.with_index do |sym, ix|
      Sequel.function(:count).*.filter(result: ix).as(sym)
    end
  end

  def self.total_count
    count_columns.count.times
      .map { |result| Sequel.function(:count).*.filter(result: result) }
      .sum.as(:total_count)
  end

  # @api private
  class Fen
    def self.for(fen, castle, active_colour, en_passant)
      case fen
      when 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR' then Initial
      else Normal
      end.new(fen, castle, active_colour, en_passant)
    end

    def initialize(fen, castle, active_colour, en_passant)
      @fen = fen
      @castle = castle
      @active_colour = active_colour
      @en_passant = en_passant
    end

    attr_reader(* %i[fen castle active_colour en_passant])

    def popular_moves(db)
      db[:moves]
        .select(:result, Sequel.as(next_san_column, :next_san))
        .join(:games, [[:id, :game_id]])
        .exclude(next_san: nil)
        .where(condition)
        .from_self(alias: 'counts')
        .select(:next_san, * Repository.counts, Repository.total_count)
        .group(:next_san)
        .order(Sequel.desc(:total_count))
    end

    # @api private
    class Initial < Fen
      def condition
        { fullmove_number: 1 }
      end

      def next_san_column
        :san
      end
    end

    # @api private
    class Normal < Fen
      def condition
        {
          fen_position: fen,
          castling_availability: castle,
          active_colour: active_colour.to_s,
          en_passant: en_passant
        }
      end

      def next_san_column
        :next_san
      end
    end
  end
  private_constant :Fen
end
