require "test/unit"

#noinspection RubyInstanceMethodNamingConvention
class QUnionTest < Test::Unit::TestCase
  def test_connections_are_reflective
    board = QUnion0.new(1)
    assert board.connect(0, 0) == false
  end

  def test_connections_are_symmetric
    board = QUnion0.new(2)
    assert board.connect(0, 1)
    assert board.connect(0, 1) == false
    assert board.connect(1, 0) == false
  end

  def test_connection_are_transitive
    board = QUnion0.new(3)
    assert board.connect(0, 1)
    assert board.connect(1, 2)
    assert board.connect(0, 2) == false
    assert board.connect(2, 0) == false
  end

  def test_book_example
    board = QUnion0.new(10)
    %w{ 3-4 4-9 8-0 2-3 5-6 2-9 5-9 7-3 4-8 5-6 0-2 6-1 }.map { |token| token.split(/-/).map{|s| s.to_i }}.each { |pair|
      board.connect(pair[0], pair[1])
    }
    (0..9).each {|i|
      assert board.connect(0, i) == false
    }
  end
end

class QUnion0
  def initialize size
    @data = Array.new(size) { |i| i }
  end

  def connect p1, p2
    if root_of(p1) == root_of(p2)
      false
    else
      @data[root_of(p1)] = root_of(p2) # didn't use roots
      true
    end
  end

  private

  def root_of pos
    return pos if @data[pos] == pos
    root_of(@data[pos])
  end
end