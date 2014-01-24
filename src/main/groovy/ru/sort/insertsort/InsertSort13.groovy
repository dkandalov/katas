package ru.sort.insertsort

import org.junit.Test

class InsertSort13 {
  @Test void "sort a list with insert sort"() {
    assert sorted([]) == []
    assert sorted([1]) == [1]
    [1, 2].permutations().each { assert sorted(it) == [1, 2] }
    [1, 2, 3].permutations().each { assert sorted(it) == [1, 2, 3] }
    [1, 2, 3, 4].permutations().each { assert sorted(it) == [1, 2, 3, 4] }
    [1, 2, 3, 4, 5].permutations().each { assert sorted(it) == [1, 2, 3, 4, 5] }
  }

  private static List sorted(List list) {
    for (int i = 1; i < list.size(); i++) {
      int j = i
      while (j > 0 && list[j - 1] > list[i]) j--
      if (j != i) moveTo(j, i, list)
    }
    list
  }

  private static moveTo(int index, int fromIndex, List list) {
    list.add(index, list.remove(fromIndex))
  }
}
