require "test/unit"

class TestPermutation < Test::Unit::TestCase
  def test_finds_all_permutations_of_an_array
    assert permutations_of([]) == [[]]
    assert permutations_of([1]) == [[1]]
    assert permutations_of([1, 2]).has_same_elements_as [[1, 2], [2, 1]]
    assert permutations_of([1, 2, 3]).has_same_elements_as [
       [1, 2, 3], [1, 3, 2],
       [2, 1, 3], [2, 3, 1],
       [3, 1, 2], [3, 2, 1],
    ]

    (1..5).map { |i| (1..i).to_a }.each do |array|
      assert permutations_of(array).has_same_elements_as(array.permutation.to_a)
    end
  end

  def test_assert_has_same_elements
    assert [1, 2].has_same_elements_as [1, 2]
    assert [1, 2].has_same_elements_as [2, 1]
    assert ![1, 2].has_same_elements_as([1, 1])
  end
end

class Array
  require "set"

  def has_same_elements_as that
    self.size == that.size && self.to_set == that.to_set
  end
end