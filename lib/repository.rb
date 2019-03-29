require 'yaml'
require 'sequel'

# Repository for database access
class Repository
  def initialize(config)
    @db = Sequel.connect(adapter: 'postgres', **config.database[:development])
  end

  def moves_in_game(game_id:)
    @db[:moves]
      .select(*game_columns)
      .where(game_id: game_id)
      .order(:fullmove_number)
      .order_append(:active_colour)
  end

  def popular_moves(fen:)
    Fen.for(fen).popular_moves(@db)
  end

  private

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
    def self.for(fen)
      case fen
      when 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR' then Initial
      else Normal
      end.new(fen)
    end

    def initialize(fen)
      @fen = fen
    end
    attr_reader :fen

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
        { fen_position: fen }
      end

      def next_san_column
        :next_san
      end
    end
  end
  private_constant :Fen
end