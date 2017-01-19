require "test/unit"

class MergeSort0 < Test::Unit::TestCase
  def test__should_sort_array_using_merge_sort
    assert_equal sort([]), []
    assert_equal sort([1]), [1]
    assert_equal sort([1, 1]), [1, 1]
    assert_equal sort([1, 2]), [1, 2]
    assert_equal sort([2, 1]), [1, 2]
    assert_equal sort([2, 3, 1]), [1, 2, 3]
    assert_equal sort([2, 3, 3, 1]), [1, 2, 3, 3]
    assert_equal sort([4, 2, 3, 3, 1]), [1, 2, 3, 3, 4]
  end

  def sort array
    return array if array.size <= 1 # didn't have "return"

    a1 = array[0..(array.size - 1)/2] # spent most time trying to split array correctly
    a2 = array[(array.size - 1)/2 + 1 .. -1] # forgot to add "+1"
    merge(sort(a1), sort(a2))
  end

  def merge a1, a2
    return a1 if a2.size == 0 # didn't have "returns"
    return a2 if a1.size == 0

    if a1[0] <= a2[0]
      [a1[0]] + merge(a1[1..-1], a2)
    else
      [a2[0]] + merge(a1, a2[1..-1])
    end
  end
end