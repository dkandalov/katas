require "rspec"

describe "binary search tree" do

  it "should determine if it contains an element" do
    bst = EMPTY
    bst.contains?(0).should == false
    bst.contains?(1).should == false
    bst.contains?(2).should == false

    bst = BST.new(1)
    bst.contains?(0).should == false
    bst.contains?(1).should == true
    bst.contains?(2).should == false

    bst = BST.new(1, BST.new(0))
    bst.contains?(0).should == true
    bst.contains?(1).should == true
    bst.contains?(2).should == false

    bst = BST.new(1, BST.new(0), BST.new(2))
    bst.contains?(0).should == true
    bst.contains?(1).should == true
    bst.contains?(2).should == true
  end

  it "should be possible to add an element" do
    bst = EMPTY.add(1)
    bst.contains?(0).should == false
    bst.contains?(1).should == true
    bst.contains?(2).should == false

    bst = BST.new(1).add(0)
    bst.contains?(0).should == true
    bst.contains?(1).should == true
    bst.contains?(2).should == false

    bst = BST.new(1, BST.new(0)).add(2)
    bst.contains?(0).should == true
    bst.contains?(1).should == true
    bst.contains?(2).should == true
  end

  it "should be possible to remove an element" do
    bst = EMPTY.remove(0)
    bst.contains?(0).should == false

    bst = BST.new(1).remove(1)
    bst.contains?(1).should == false


    bst = BST.new(1, BST.new(0)).remove(1)
    bst.contains?(0).should == true
    bst.contains?(1).should == false

    bst = BST.new(1, BST.new(0)).remove(0)
    bst.contains?(0).should == false
    bst.contains?(1).should == true


    bst = BST.new(1, EMPTY, BST.new(2)).remove(1)
    bst.contains?(1).should == false
    bst.contains?(2).should == true

    bst = BST.new(1, EMPTY, BST.new(2)).remove(2)
    bst.contains?(1).should == true
    bst.contains?(2).should == false


    bst = BST.new(2, BST.new(1, BST.new(0))).remove(2)
    bst.contains?(0).should == true
    bst.contains?(1).should == true
    bst.contains?(2).should == false
  end

  it "should satisfy property based assertions" do
    # all added elements are contained in BST
    (1..5).to_a.permutation.to_a.each do |array|
      bst = EMPTY
      array.each { |element| bst = bst.add(element) }
      array.each { |element| bst.contains?(element).should == true }
    end

    (1..6).to_a.permutation.to_a.each do |array|
      bst = EMPTY
      array_head = array[0..1]
      array_tail = array[2..-1]

      array.each { |element| bst = bst.add(element) }
      array_tail.each { |element| bst = bst.remove(element) }

      array_head.each { |element| bst.contains?(element).should == true }
      array_tail.each { |element| bst.contains?(element).should == false }
    end
  end


  class EmptyBST
    #noinspection RubyUnusedLocalVariable
    def contains?(value)
      false
    end

    def add(value)
      BST.new(value)
    end

    def remove(value)
      self
    end
  end
  EMPTY = EmptyBST.new

  class BST
    def initialize(value = nil, left = EMPTY, right = EMPTY)
      @value = value
      @left = left
      @right = right
    end

    def remove(value)
      if value == @value
        if @left != EMPTY
          biggest_value, new_left = @left.remove_biggest
          BST.new(biggest_value, new_left, @right)
        elsif @right != EMPTY
          smallest_value, new_right = @right.remove_smallest
          BST.new(smallest_value, @left, new_right)
        else
          EMPTY
        end
      elsif value < @value
        BST.new(@value, @left.remove(value), @right)
      elsif value > @value
        BST.new(@value, @left, @right.remove(value))
      else
        self
      end
    end

    def contains?(value)
      return true if value == @value
      return @left.contains?(value) if @left and value < @value
      return @right.contains?(value) if @right and value > @value
      false
    end

    def add(value)
      if value == @value
        self
      elsif value < @value
        BST.new(@value, @left.add(value), @right)
      elsif value > @value
        BST.new(@value, @left, @right.add(value))
      end
    end

    def remove_biggest
      if @right == EMPTY
        [@value, @left]
      else
        result, new_right = @right.remove_biggest
        [result, BST.new(@value, @left, new_right)]
      end
    end

    def remove_smallest
      if @left == EMPTY
        [@value, @right]
      else
        result, new_left = @left.remove_smallest
        [result, BST.new(@value, new_left, @right)]
      end
    end
  end

end