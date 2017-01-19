require 'test/unit'

class TestChildTracker < Test::Unit::TestCase

  def test_children
    assert_equal  [], ChildTracker.children
    c1 = ChildTracker.new
    assert_equal [c1], ChildTracker.children
    c2 = ChildTracker.new
    assert_equal [c1, c2], ChildTracker.children
  end

end


class ChildTracker
  @children = []

  #self.class.attr_accessor :children

  class << self
    attr_accessor :children
  end

  def initialize
    ChildTracker.children << self
  end
end