require "test/unit"

class BSearchTest < Test::Unit::TestCase
  def test_binary_search
    assert_equal -1, bsearch(1, [])
    assert_equal [-1, 0, -1], [0, 1, 2].map { |n| bsearch(n, [1]) }
    assert_equal [-1, 0, 1, -1], [0, 1, 2, 3].map { |n| bsearch(n, [1, 2]) }
    assert_equal [-1, 0, 1, 2, -1], [0, 1, 2, 3, 4].map { |n| bsearch(n, [1, 2, 3]) }
    assert_equal [-1, 0, 1, 2, 3, -1], [0, 1, 2, 3, 4, 5].map { |n| bsearch(n, [1, 2, 3, 4]) }
  end

  def bsearch value, list, shift = 0
    return -1 if list.empty?

    mid_pos = list.size / 2
    mid_value = list[mid_pos]

    if value == mid_value
      shift + mid_pos
    elsif list.size == 1
      -1
    elsif value < mid_value
      bsearch value, list[0..mid_pos - 1], shift # used mid_value instead of mid_pos
    else
      bsearch value, list[(mid_pos + 1)..-1], shift + mid_pos + 1
    end
  end

  def bsearch_ value, list
    from = 0
    to = list.size # used value instead of list
    while from < to
      mid_pos = (from + to) / 2
      mid_value = list[mid_pos]

      if value == mid_value
        return mid_pos
      elsif value < mid_value
        to = mid_pos # was "mid_pos - 1"
      elsif value > mid_value
        from = mid_pos + 1
      end
    end
    -1
  end
end