require_relative "perm_test.rb"

# arcade
class TestPermutation
  def permutations_of array
    return [[]] if array.empty?
    return [array] if array.size == 1

    (0...array.size).map {|i| {removed: array[i], sub_array: array - [array[i]]} }.flat_map { |t|
      permutations_of(t[:sub_array]).map { |it|
        [t[:removed]] + it
      }
    }
  end
end
