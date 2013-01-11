require "rspec"

def binary_search(element, array, shift = 0)
  return -1 if array.empty?

  mid_index = array.size / 2
  mid_element = array[mid_index]

  if element == mid_element
    shift + mid_index
  elsif element < mid_element
    binary_search(element, array[0...mid_index], shift)
  elsif element > mid_element
    binary_search(element, array[(mid_index + 1)..-1], shift + mid_index + 1)
  end
end

describe "binary search" do
  it "given a list should find index of an element" do
    binary_search(1, []).should == -1

    binary_search(0, [1]).should == -1
    binary_search(1, [1]).should == 0
    binary_search(2, [1]).should == -1

    binary_search(0, [1, 2]).should == -1
    binary_search(1, [1, 2]).should == 0
    binary_search(2, [1, 2]).should == 1
    binary_search(3, [1, 2]).should == -1

    binary_search(0, [1, 2, 3]).should == -1
    binary_search(1, [1, 2, 3]).should == 0
    binary_search(2, [1, 2, 3]).should == 1
    binary_search(3, [1, 2, 3]).should == 2
    binary_search(4, [1, 2, 3]).should == -1

    binary_search(0, [1, 2, 3, 4]).should == -1
    binary_search(1, [1, 2, 3, 4]).should == 0
    binary_search(2, [1, 2, 3, 4]).should == 1
    binary_search(3, [1, 2, 3, 4]).should == 2
    binary_search(4, [1, 2, 3, 4]).should == 3
    binary_search(5, [1, 2, 3, 4]).should == -1
  end

  it "should find index of element for arrays with numbers up to size of 10" do
    (0..10).each do |number_of_elements|
      (0..number_of_elements).each do |i|
        array = (1..i).to_a

        binary_search(0, array).should == -1
        array.each { |element| binary_search(element, array).should == element - 1 }
        binary_search(i + 1, array).should == -1
      end
    end
  end
end