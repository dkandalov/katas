require "rspec"

describe "Insert sort" do
  it "sorts things" do
    sorted([]).should == []
    sorted([1]).should == [1]
    [1, 2, 3].permutation.each { |it| sorted(it).should == [1, 2, 3] }
    [1, 2, 3, 4].permutation.each { |it| sorted(it).should == [1, 2, 3, 4] }
  end

  def sorted(list)
    return list if list.size < 2
    (1...list.size).each do |i|
      j = find_insert_index(i) { |it| list[it] <= list[i]}
      swap_elements_in(list, i, j)
    end
    list
  end

  def find_insert_index(from_index, &predicate)
    j = from_index - 1
    while j >= 0 and not predicate.call(j) do
      j = j - 1
    end
    j + 1
  end

  def swap_elements_in(list, from, to)
    element = list.delete_at(from)
    list.insert(to, element)
  end
end