require 'test/unit'

module BSearch2
  def bsearch v, values
    from = 0
    to = values.size

    while from != to
      mid_index = (from + to) /2
      mid_value = values[mid_index]

      return mid_index if mid_value == v

      if mid_value > v
        to = mid_index;
      elsif mid_value < v
        from = mid_index + 1
      end
    end

    return -1
  end
end

class BSearch2Test < Test::Unit::TestCase
  include BSearch2
  
  def test_bsearch
    assert_equal bsearch(1, []), -1

    assert_equal bsearch(1, [1]), 0

    assert_equal bsearch(0, [1, 2]), -1
    assert_equal bsearch(1, [1, 2]), 0
    assert_equal bsearch(2, [1, 2]), 1
    assert_equal bsearch(3, [1, 2]), -1

    assert_equal bsearch(0, [1, 2, 3]), -1
    assert_equal bsearch(1, [1, 2, 3]), 0
    assert_equal bsearch(2, [1, 2, 3]), 1
    assert_equal bsearch(3, [1, 2, 3]), 2
    assert_equal bsearch(4, [1, 2, 3]), -1

    assert_equal bsearch(12, [8, 12, 15]), 1
  end
end

