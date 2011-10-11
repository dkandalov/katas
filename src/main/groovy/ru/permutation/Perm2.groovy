package ru.permutation

import org.junit.Test

/**
 * User: DKandalov
 */
class Perm2 {
  @Test
  public void shouldFindAllPermutationsForList() {
    assert perm([]) == []
    assert perm([1]) == [[1]]
    assert perm([1, 2]) == [[1, 2], [2, 1]] // didn't consider that output will start with shifted value (changed algorithm to match tests)
    assert perm([1, 2, 3]).sort() == [
            [1, 2, 3], [2, 1, 3], [2, 3, 1],
            [1, 3, 2], [3, 1, 2], [3, 2, 1]
    ].sort()
    assert perm([1, 2, 3, 4]) == [
                    [1, 2, 3, 4], [1, 2, 4, 3], [1, 3, 2, 4], [1, 3, 4, 2], [1, 4, 2, 3], [1, 4, 3, 2],
                    [2, 1, 3, 4], [2, 1, 4, 3], [2, 3, 1, 4], [2, 3, 4, 1], [2, 4, 1, 3], [2, 4, 3, 1],
                    [3, 1, 2, 4], [3, 1, 4, 2], [3, 2, 1, 4], [3, 2, 4, 1], [3, 4, 1, 2], [3, 4, 2, 1],
                    [4, 1, 2, 3], [4, 1, 3, 2], [4, 2, 1, 3], [4, 2, 3, 1], [4, 3, 1, 2], [4, 3, 2, 1]
            ]
  }

  static List<List> perm(List list) {
    if (list.size() == 0) return []
    if (list.size() == 1) return [list]

    def result = []
    list.size().times { i ->
      perm(listWithout(i, list)).each { result << [list[i]] + it } // didn't flatten properly
    }
    result
  }

  static List listWithout(int i, List list) {
    def result = new LinkedList(list)
    result.remove(i)
    result
  }
}
