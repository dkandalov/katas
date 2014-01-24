package ru.sort.insertsort

import org.junit.Test

class InsertSort13 {
  @Test void "sort a list with insert sort"() {
    assert sorted([]) == []
    assert sorted([1]) == [1]
    [1, 2, 3, 4, 5].permutations().each { permutation ->
      assert sorted(permutation) == [1, 2, 3, 4, 5]
    }
  }

  private static List sorted(List list) {
    for (int i = 1; i < list.size(); i++) {
      for (int j = i; j > 0 && list[j - 1] > list[j]; j--) {
        swap(j - 1, j, list)
      }
    }
    list
  }

  private static def swap(int i, int j, List list) {
    def value = list[i]
    list[i] = list[j]
    list[j] = value
  }
}
