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
      int j = i - 1
      while (j >= 0 && list[j] > list[i]) j--
      if (list[j + 1] > list[i]) {
        def element = list.remove(i as int)
        list.add(j + 1, element)
      }
    }
    list
  }
}
