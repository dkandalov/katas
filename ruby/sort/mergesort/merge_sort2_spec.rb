require "rspec"

describe "Merge sort" do
  it "should sort array" do
    sort([]).should == []
    sort([1]).should == [1]

    sort([1, 2]).should == [1, 2]
    sort([2, 1]).should == [1, 2]

    sort([1, 2, 3]).should == [1, 2, 3]
    sort([1, 3, 2]).should == [1, 2, 3]
    sort([2, 1, 3]).should == [1, 2, 3]
    sort([2, 3, 1]).should == [1, 2, 3]
    sort([3, 1, 2]).should == [1, 2, 3]
    sort([3, 2, 1]).should == [1, 2, 3]

    (1..5).to_a.permutation.each { |array|
      sort(array).should == [1, 2, 3, 4, 5]
    }
  end

  def sort(array)
    return array if array.size < 2

    part1, part2 = split(array)
    merge(sort(part1), sort(part2))
  end

  private

  def split(array)
    part1 = array[0...(array.size / 2)]
    part2 = array[(array.size / 2)..array.size]
    [part1, part2]
  end

  def merge(part1, part2)
    return part1 if part2.empty?
    return part2 if part1.empty?

    i1 = 0
    i2 = 0
    result = []
    while i1 < part1.size and i2 < part2.size
      if part1[i1] <= part2[i2]
        result << part1[i1]
        i1 += 1
      else
        result << part2[i2]
        i2 += 1
      end
    end
    result + part1[i1..part1.size] + part2[i2..part2.size]
  end
end