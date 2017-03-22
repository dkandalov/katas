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
    (1..<list.size()).each { index ->
      def insertToIndex = (0..index - 1).find{ list[it] > list[index] }
      if (insertToIndex != null) moveTo(insertToIndex, index, list)
    }
    list
  }

  private static moveTo(int index, int fromIndex, List list) {
    //noinspection GroovyAssignabilityCheck
    list.add(index, list.remove(fromIndex))
  }
}
