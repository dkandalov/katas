package katas.groovy.sort.insertsort

import org.junit.Test

/**
 * @author DKandalov
 */
class InsertSort6 {
  @Test
  public void shouldSortList() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]
    assert sort([1, 2, 3]) == [1, 2, 3]
    assert sort([1, 3, 2]) == [1, 2, 3]
    assert sort([2, 1, 3]) == [1, 2, 3]

    [1, 2, 3, 4, 5].permutations().each { list ->
      assert sort(list) == [1, 2, 3, 4, 5]
    }
  }

  static def sort(list) {
    if (list.size == 1) return list

    for (int i = 1; i < list.size(); i++) {
      for (int j = i; j > 0; j--) {
        if (list[j] >= list[j - 1]) break
        exchange(j, j - 1, list)
      }
    }
    list
  }

  static def exchange(i, j, list) {
    def tmp = list[i]
    list[i] = list[j]
    list[j] = tmp
  }
}
