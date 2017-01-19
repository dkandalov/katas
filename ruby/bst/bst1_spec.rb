require "rspec"

describe "Binary search tree" do

  it "should determine if element is in the tree" do
    bst = BST.new(0)
    bst.contains?(0).should == true
    bst.contains?(1).should == false

    bst = BST.new(1, BST.new(0, BST.new(-1)), BST.new(2))
    bst.contains?(1).should == true
    bst.contains?(0).should == true
    bst.contains?(-1).should == true
    bst.contains?(-2).should == false
    bst.contains?(2).should == true
    bst.contains?(3).should == false
  end

  it "should add element to a tree" do
    bst = BST.new(0)
    bst.contains?(1).should == false

    bst.add(1).should == true
    bst.contains?(1).should == true

    bst.add(1).should == false
  end

  it "should remove element from tree" do
    bst = BST.new(1, BST.new(0), BST.new(2))
    bst.contains?(0).should == true
    bst.contains?(1).should == true
    bst.contains?(2).should == true

    bst.remove(0).should == true
    bst.contains?(0).should == false
    bst.contains?(1).should == true
    bst.contains?(2).should == true
    bst.remove(0).should == false

    bst.remove(1).should == true
    bst.contains?(0).should == false
    bst.contains?(1).should == false
    bst.contains?(2).should == true
    bst.remove(1).should == false

    bst.remove(2).should == true
    bst.contains?(0).should == false
    bst.contains?(1).should == false
    bst.contains?(2).should == false
    bst.remove(2).should == false
  end

  it "should have all operations on empty tree" do
    bst = BST.new()

    bst.contains?(1).should == false
    bst.remove(1).should == false
    bst.add(1).should == true
  end

  #
  # This is a mess
  #
  class BST
    attr_reader :value, :left, :right

    def initialize(value = nil, left = nil, right = nil)
      @value = value
      @left = left
      @right = right
    end

    def remove(value)
      return false if @value.nil?

      if value == @value
        if @right != nil
          @value = @right.remove_smallest_value
        elsif @left != nil
          @value = @left.remove_biggest_value
        else
          @value = nil
        end
      elsif value < @value
        return false if @left.nil?
        @left.remove(value)
      elsif value > @value
        return false if @right.nil?
        @right.remove(value)
      end

      @left = nil if @left and @left.value.nil?
      @right = nil if @right and @right.value.nil?

      true
    end

    def add(value)
      if @value.nil?
        @value = value
        return true
      end

      if value < @value
        if @left.nil?
          @left = BST.new(value)
          true
        else
          @left.add(value)
        end
      elsif value > @value
        if @right.nil?
          @right = BST.new(value)
          true
        else
          @right.add(value)
        end
      else
        value != @value
      end
    end

    def contains?(value)
      return false if @value.nil?

      if value < @value and @left != nil
        @left.contains?(value)
      elsif value > @value and @right != nil
        @right.contains?(value)
      else
        @value == value
      end
    end

    #private

    def remove_smallest_value
      if @left != nil and @left.left.nil?
        result = @left.value
        @left = nil
        result
      elsif @left != nil
        @left.remove_smallest_value
      else
        result = @value
        @value = nil
        result
      end
    end

    def remove_biggest_value
      if @right != nil and @right.right.nil?
        result = @right.value
        @right = nil
        result
      elsif @right != nil
        @right.remove_biggest_value
      else
        result = @value
        @value = nil
        result
      end
    end

  end
end