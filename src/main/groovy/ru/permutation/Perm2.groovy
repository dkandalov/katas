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

  static List<List> perm_non_recursive(List list) {
    if (list.empty) return []

    def circularList = new CircularList(list)
    def result = [circularList.copy()]
    (factorial(circularList.size()) - 1).times { i ->
      circularList.swap(i, i + 1)
      result << circularList.copy()
    }
    result
  }

  @Test
  public void shouldCalculateFactorial()
  {
      assert factorial(0) == 0
      assert factorial(1) == 1
      assert factorial(2) == 2
      assert factorial(3) == 6
  }

  static long factorial(int n) {
    if (n == 0) return 0
    if (n == 1) return 1
    (long) (2..n).inject(1) { acc, i -> acc * i }
  }

  private static class CircularList {
    List list

    CircularList(list) {
      this.list = list
    }

    def swap(int i1, int i2) {
      if (i1 >= list.size()) i1 = i1 % list.size()
      if (i2 >= list.size()) i2 = i2 % list.size()
      def tmp = list[i1]
      list[i1] = list[i2]
      list[i2] = tmp
    }

    List copy() {
      new ArrayList(list)
    }

    int size() {
      list.size()
    }
  }
}
