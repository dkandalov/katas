package katas.groovy.bsearch

import org.junit.Test

/**
 * User: dima
 * Date: 26/12/2011
 */
class BSearch9 {
  @Test public void shouldFindIndexOfElementInAList() {
    assert bsearch(1, []) == -1
    assert [0, 1, 2].collect{ bsearch(it, [1]) } == [-1, 0, -1]
    assert [0, 1, 2, 3].collect{ bsearch(it, [1, 2]) } == [-1, 0, 1, -1]
    assert [0, 1, 2, 3, 4].collect{ bsearch(it, [1, 2, 3]) } == [-1, 0, 1, 2, -1]
    assert [0, 1, 2, 3, 4, 5].collect{ bsearch(it, [1, 2, 3, 4]) } == [-1, 0, 1, 2, 3, -1]
  }

  static def bsearch(int value, List list) {
    int from = 0
    int to = list.size() - 1

    while (from <= to) { // wrong order in condition :(
      def midPos = (to + from).intdiv(2)
      if (value < list[midPos]) {
        to = midPos - 1
      } else if (value > list[midPos]) {
        from = midPos + 1
      } else {
        return midPos
      }
    }

    return -1
  }

  static def bsearch_(int value, List list) {
    int from = 0
    int to = list.size()

    while (from != to) {
      def midPos = (to + from).intdiv(2)
      if (value < list[midPos]) {
        to = midPos
      } else if (value > list[midPos]) {
        from = midPos + 1
      } else {
        return midPos
      }
    }

    return -1
  }
}
