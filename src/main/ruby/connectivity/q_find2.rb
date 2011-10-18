require "test/unit"

class QFind2Test < Test::Unit::TestCase
  def test_should_determine_if_two_points_are_connected
    board = QFind2.new(2)
    assert !board.are_connected?(0, 1)
    board.connect(0, 1)
    assert board.are_connected?(0, 1)
  end

  def test_connections_should_be_transitive
    board = QFind2.new(3)
    board.connect(0, 1)
    board.connect(1, 2)
    assert board.are_connected?(0, 2)
  end

  def test_big_test_case
    board = QFind2.new(10)
    board.connect_all %w{ 3-4 4-9 8-0 2-3 5-6 2-9 5-9 7-3 4-8 5-6 0-2 6-1 }
    assert (0..9).all? {|i|
      board.are_connected?(0, i)
    }
  end
end

class QFind2
  def initialize size
    @data = Array.new(size) { |i| i }
  end

  def connect p1, p2
    tmp = @data[p1]
    @data.map! {|p| p == tmp ? @data[p2]: p }
  end

  def connect_all connections
    connections.each { |c|
      c = c.split "-"               # didn't realize that I need parse strings :(
      connect(c[0].to_i, c[1].to_i)
    }
  end

  def are_connected? p1, p2
    @data[p1] == @data[p2]
  end
end
