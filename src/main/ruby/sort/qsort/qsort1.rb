def qsort values
  return values if values.length <= 1

  pivot_index = values.length / 2
  pivot = values.delete_at(pivot_index)

  less = values.select {|v| v < pivot}
  greater = values.select {|v| v > pivot}

  qsort(less) + [pivot] + qsort(greater)
end

p qsort []
p qsort [1]
p qsort [2, 1]
p qsort [2, 3, 1]
p qsort [4, 5, 2, 3, 1]