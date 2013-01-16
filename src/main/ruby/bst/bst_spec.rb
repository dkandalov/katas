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

    bst.remove(0).should == true
    bst.contains?(0).should == false

    bst.remove(0).should == false

    #TODO unfinished
  end

  it "should have all operations on empty tree" do
    # TODO
  end

  class BST
    attr_reader :value

    def initialize(value, left = nil, right = nil)
      @value = value
      @left = left
      @right = right
    end

    def remove(value)
      if @left != nil and @left.value == value
        @left = nil
        true
      elsif @right != nil and @right.value == value
        @right = nil
        true
      else
        false
      end
    end

    def add(value)
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
      if value < @value and @left != nil
        @left.contains?(value)
      elsif value > @value and @right != nil
        @right.contains?(value)
      else
        @value == value
      end
    end
  end
end