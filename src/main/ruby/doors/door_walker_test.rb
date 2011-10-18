require "test/unit"

class TestSimpleNumber < Test::Unit::TestCase
  def test_door_walker
    assert_equal(walk_doors(0), [])
    assert_equal(walk_doors(1), [true])
    assert_equal(walk_doors(2), [true, false])
    assert_equal(walk_doors(3), [true, false, false])
    assert_equal(walk_doors(4), [true, false, false, true])
    assert_equal(walk_doors(5), [true, false, false, true, false])
    assert_equal(walk_doors(6), [true, false, false, true, false, false])
    assert_equal(walk_doors(7), [true, false, false, true, false, false, false])
    assert_equal(walk_doors(8), [true, false, false, true, false, false, false, false])
    assert_equal(walk_doors(9), [true, false, false, true, false, false, false, false, true])
    walk_doors(100).each_with_index { |v, i|
      p i if v
    }
  end

  def walk_doors(doors_number)
    doors = (1..doors_number).collect { false }

    (1..doors_number).each do |step_size|
      j = step_size - 1
      while (j < doors_number)
        doors[j] = !doors[j]
        j = j + step_size
      end
    end

    doors
  end

end