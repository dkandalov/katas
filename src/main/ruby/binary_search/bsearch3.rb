require "test/unit"

module BSearch3
  def search value, values
    from = 0
    to = values.size

    while from < to
      mid_index = (from + to) / 2
      mid_value = values[mid_index]

      return mid_index if value == mid_value
      to = mid_index if value < mid_value
      from = mid_index + 1 if value > mid_value
    end

    return -1
  end
end

class TestBSearch3 < Test::Unit::TestCase
  include BSearch3

  def test_binary_search
    assert_equal -1, search(0, [])

    assert_equal -1, search(1, [2])
    assert_equal 0, search(2, [2])
    assert_equal -1, search(3, [2])

    assert_equal -1, search(1, [2, 3])
    assert_equal 0, search(2, [2, 3])
    assert_equal 1, search(3, [2, 3])
    assert_equal -1, search(4, [2, 3])
  end
end