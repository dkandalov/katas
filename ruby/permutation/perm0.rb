require "test/unit"

class PermTest < Test::Unit::TestCase
  def test_permutation
    assert_equal [], perm([])
    assert_equal [[1]], perm([1])
    assert_equal [[1, 2], [2, 1]], perm([1, 2])
    assert_equal perm([1, 2, 3]), [
        [1, 2, 3], [1, 3, 2],
        [2, 1, 3], [2, 3, 1],
        [3, 1, 2], [3, 2, 1],
    ]
    assert [1, 2, 3, 4].permutation, perm([1, 2, 3, 4])
    assert [1, 2, 3, 4, 5].permutation, perm([1, 2, 3, 4, 5])
  end

  def perm values
    return [] if values.empty?
    return [values] if values.size == 1

    result = []
    values.each { |n|
      perm(values.clone - [n]).each { |r| result << [n] + r }
    }
    result
  end
end