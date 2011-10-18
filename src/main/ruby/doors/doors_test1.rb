require "test/unit"

class DoorsTest1 < Test::Unit::TestCase
  def test_that_doors_are_visited_properly
    assert_equal doors(1), [false]
    assert_equal doors(2), [false, true]
    assert_equal doors(3), [false, true, true]
    assert_equal doors(4), [false, true, true, false]
    assert_equal doors(5), [false, true, true, false, true]
    assert_equal doors(6), [false, true, true, false, true, true]
    assert_equal doors(7), [false, true, true, false, true, true, true]
    assert_equal doors(8), [false, true, true, false, true, true, true, true]
    assert_equal doors(9), [false, true, true, false, true, true, true, true, false]
    assert_equal doors(10), [false, true, true, false, true, true, true, true, false, true]
    doors(100).each_with_index { |v, i|
      p i if !v
    }
  end

  def doors doors_amount
    doors = Array.new(doors_amount) { false }

    (2..doors_amount).each { |step_size|
      from = step_size - 1
      to = (doors_amount - (doors_amount % step_size)) # didn't consider that Range.step will include last end of range anyway
      (from .. to).step(step_size) { |i| doors[i] = !doors[i] } # typo in "doors" variable
    }
    doors
  end

  def doors2 doors_amount # this is wrong its not going to work
    f = fibonacci_up_to doors_amount
    Array.new(doors_amount) { |i| (f.include? i) ? false : true }
  end

  def fibonacci_up_to n
    result = []
    i = 1
    f = 0
    while f < n
      f = fibonacci(i)
      result << f
      i = i + 1
    end
    result
  end

  def fibonacci i
    return 1 if i == 0
    return 1 if i == 1
    fibonacci(i - 1) + fibonacci(i - 2)
  end
end