module BSearch1
  def bsearch value, values
    bsearch_ value, values, 0, values.length - 1
  end

  def bsearch_ value, values, from, to
    return -1 if from > to

    mid_position = (from + to) / 2
    mid_value = values[mid_position]

    return mid_position if value == mid_value

    return bsearch_ value, values, from, mid_position - 1 if value < mid_value
    return bsearch_ value, values, mid_position + 1, to if value > mid_value
  end
end

include BSearch1

p bsearch 0, [1]
p bsearch 1, [1]
p bsearch 2, [1]

p bsearch 0, [1, 2, 3, 4, 5]
p bsearch 1, [1, 2, 3, 4, 5]
p bsearch 2, [1, 2, 3, 4, 5]
p bsearch 3, [1, 2, 3, 4, 5]
p bsearch 4, [1, 2, 3, 4, 5]
p bsearch 5, [1, 2, 3, 4, 5]
p bsearch 6, [1, 2, 3, 4, 5]

