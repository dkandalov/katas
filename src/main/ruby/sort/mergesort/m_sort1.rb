require "test/unit"

class MergeSort1 < Test::Unit::TestCase
  def test_should_sort_arrays
    assert_equal [].m_sort, []
    assert_equal [1].m_sort, [1]
    assert_equal [1, 2].m_sort, [1, 2]
    assert_equal [2, 1].m_sort, [1, 2]
    assert_equal [3, 1, 2].m_sort, [1, 2, 3]
    assert_equal [3, 4, 1, 2].m_sort, [1, 2, 3, 4]
    assert_equal [4, 2, 3, 3, 1].m_sort, [1, 2, 3, 3, 4]

    (1..7).each do |n|
      (1..n).to_a.permutation.each do |array|
        assert_equal array.m_sort, (1..n).to_a
      end
    end
  end

  def self.m_sort array
    return array if array.size <= 1

    part1, part2 = split(array)
    merge(m_sort(part1), m_sort(part2))
  end

  def self.split(array)
    mid_pos = (array.size - 1) / 2
    return array[0..mid_pos], array[mid_pos+1..-1]
  end

  def self.merge(part1, part2)
    return part2 if part1.empty?
    return part1 if part2.empty?

    if part1[0] > part2[0] then
      return [part2[0]] + merge(part1, part2.last(part2.size - 1))
    end
    if part1[0] <= part2[0] then
      return [part1[0]] + merge(part1.last(part1.size - 1), part2)
    end
  end
end

class Array
  def m_sort
    MergeSort1.m_sort(self)
  end
end
