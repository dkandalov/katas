require "test/unit"

class MainTest < Test::Unit::TestCase
  def test_ssort
    assert_equal [], s_sort([])
    assert_equal [1], s_sort([1])
    assert_equal [1, 2], s_sort([2, 1])
    assert_equal [1, 2, 3], s_sort([2, 3, 1])
    assert_equal [1, 2, 3, 4], s_sort([2, 4, 3, 1])
  end
end

def s_sort a
  a.size.times do |n|
    min = 0xFFFFFFFFFFFFFFFF
    min_index = -1

    a[n..a.size-1].each_with_index do |v, i|
      if v < min then
        min = v
        min_index = i + n
      end
    end

    a[min_index] = a[n]
    a[n] = min
  end

  return a
end