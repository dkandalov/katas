require "test/unit"

class BSTTest < Test::Unit::TestCase
  def test_bst
    assert_equal(nil, BST.new.get("a"))

    assert_equal(1, BST.new.add_all({"a" => 1, "b" => 2}).get("a"))

    assert_equal(1, BST.new.add_all({"a" => 1, "b" => 2}).get("a"))
    assert_equal(2, BST.new.add_all({"a" => 1, "b" => 2}).get("b"))
    assert_equal(3, BST.new.add_all({"a" => 1, "b" => 2, "c" => 3}).get("c")) # was wrong test condition
    assert_equal(nil, BST.new.add_all({"a" => 1, "b" => 2}).get("d"))
  end

  private

  class BST
    def add_all entries
      entries.each { |key, value| @root = do_add(key, value, @root) }
      self
    end

    def get key
      do_get(key, @root) # forgot to implement
    end

    private

    def do_get key, node
      if node.nil?
        nil
      else
        if key == node.key
          node.value
        elsif key > node.key
          do_get(key, node.right)
        else
          do_get(key, node.left)
        end
      end
    end

    def do_add key, value, node
      if node.nil? then
        Node.new(key, value)
      else
        if key > node.key # compared with value
          node.right = do_add(key, value, node.right)
        else
          node.left = do_add(key, value, node.left)
        end
        node
      end
    end

  end

  class Node
    attr_accessor :key, :value, :left, :right

    def initialize key, value, left = nil, right = nil
      @key = key
      @value = value
      @left = left
      @right = right
    end
  end
end
