package katas.groovy.sort.insertsort

import org.junit.Test

class InsertSort12 {
  @Test void "all sorted permutations of a list should be equal"() {
    [1, 2, 3, 4].permutations().collect { sorted(it) }.with { lists ->
      lists.each { assert it == lists.first() }
    }
    [1, 2, 3, 4, 5].permutations().collect { sorted(it) }.with { lists ->
      lists.each { assert it == lists.first() }
    }
  }

  private static List sorted(List list) {
    def result = []
    for (def value : list) {
      int i = result.size()
      while (i > 0 && value < result[i - 1]) i-- // had wrong second condition in &&
      result.add(i, value)
    }
    result
  }
}
