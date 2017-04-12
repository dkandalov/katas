package katas.groovy.sort.odd_even_mergesort

import org.junit.Test

 /**
 * User: DKandalov
 */
class OEMSort1_ { // TODO
  @Test
  public void shouldSortList() {
    assert sort([]) == []
    assert sort([0, 1]) == [0, 1]
    assert sort([1, 0]) == [0, 1]

    [1, 2, 3, 4].permutations().each { list ->
      assert sort(list) == [1, 2, 3, 4]
    }
  }

  @SuppressWarnings("GroovyVariableNotAssigned")
  static List sort(List list) {
    if (list.size() < 2) return list
    def (part1, part2) = split(list)
    merge(sort(part1) + sort(part2))
  }

  @Test
  public void shouldMergeList() {
    assert merge4([1, 2, 3, 4]) == [1, 2, 3, 4]
    assert merge4([1, 3, 2, 4]) == [1, 2, 3, 4]
    assert merge4([1, 4, 2, 3]) == [1, 2, 3, 4]

    assert merge4([2, 3, 1, 4]) == [1, 2, 3, 4]
    assert merge4([2, 4, 1, 3]) == [1, 2, 3, 4]

    assert merge4([3, 4, 1, 2]) == [1, 2, 3, 4]
  }

  static List merge4(List list) {
    compare(list, 0, 2)
    compare(list, 1, 3)
    compare(list, 1, 2)
    list
  }

  static List merge8(List list) {
    compare(list, 0, 2)
    compare(list, 1, 3)
    compare(list, 1, 2)
    list
  }

  static List merge(List list) {
    if (list.size() == 2) return compare(list, 0, 1)

    // odd / even
    compare(list, 0, 2)
    compare(list, 1, 3)

//    compare(list, 0, 1)
    compare(list, 1, 2)
//    1.step(list.size() - 3, 2) { i ->
//      compare(list, i, i + 1)
//    }
    list
  }

  static List compare(list, i, j) {
    if (list[i] > list[j]) {
      def tmp = list[i]
      list[i] = list[j]
      list[j] = tmp
    }
    list
  }

  static def split(List list) {
    int midPos = list.size() / 2
    [new ArrayList(list[0..midPos - 1]), new ArrayList(list[midPos..list.size() - 1])]
  }
}
