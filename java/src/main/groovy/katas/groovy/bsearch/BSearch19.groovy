package katas.groovy.bsearch

import org.junit.Test

class BSearch19 {
  @Test void "should find index of element in a list"() {
    assert indexOf(0, []) == -1

    assert indexOf(0, [1]) == -1
    assert indexOf(1, [1]) == 0
    assert indexOf(2, [1]) == -1

    assert indexOf(0, [1, 2]) == -1
    assert indexOf(1, [1, 2]) == 0
    assert indexOf(2, [1, 2]) == 1
    assert indexOf(3, [1, 2]) == -1

    assert indexOf(0, [1, 2, 3]) == -1
    assert indexOf(1, [1, 2, 3]) == 0
    assert indexOf(2, [1, 2, 3]) == 1
    assert indexOf(3, [1, 2, 3]) == 2
    assert indexOf(4, [1, 2, 3]) == -1
  }

  private static int indexOf(element, List list) {
    int from = 0
    int to = list.size()

    while (from < to) {
      int midIndex = (from + to) / 2
      def midElement = list[midIndex]

      if (midElement == element) {
        return midIndex
      } else if (element < midElement) {
        to = midIndex
      } else if (element > midElement) {
        from = midIndex + 1
      }
    }

    return -1
  }
}
