package katas.groovy.sort.mergesort

import org.junit.Test

/**
 * User: DKandalov
 */
class MergeSort5 {
  @Test
  public void shouldSortList() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]

    assert sort([1, 2, 3]) == [1, 2, 3]
    assert sort([2, 1, 3]) == [1, 2, 3]

    [1, 2, 3, 4, 5].permutations().each { list ->
      assert sort(list) == [1, 2, 3, 4, 5]
    }
  }

  @SuppressWarnings("GroovyVariableNotAssigned")
  static List sort(List list) {
    if (list.size() <= 1) return list
    def (part1, part2) = split(list)
    merge(sort(part1), sort(part2))
  }

  static List merge(List list1, List list2) {
    def result = []
    while (!list1.empty && !list2.empty) {
      if (list1.head() > list2.head()) {
        result << list2.get(0)
        list2.remove(0)
      } else {
        result << list1.get(0)
        list1.remove(0)
      }
    }
    result.addAll(list1) // got concurrent modification exception
    result.addAll(list2)
    result
  }

  static List split(List list) {
    int midPos = list.size() / 2
    [new ArrayList(list[0..midPos - 1]), new ArrayList(list[midPos..list.size() - 1])] // didn't create copy of arrays, which caused that above exception
  }
}
