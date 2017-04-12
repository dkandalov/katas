package katas.groovy.bsearch

import org.junit.Test

/**
 * User: dima
 * Date: 21/01/2012
 */
class BSearch10 {
  @Test public void GIVEN_listShouldFindIndexOfElement() {
    assert find(1, []) == -1
    assert find(1, [1]) == 0
    assert find(1, [1, 2]) == 0
    assert find(2, [1, 2]) == 1

    assert find(1, [1, 2, 3]) == 0
    assert find(2, [1, 2, 3]) == 1
    assert find(3, [1, 2, 3]) == 2

    (1..100).toList().each {
      assert find(it, (1..100).toList()) == it - 1
    }
  }

  def find(value, list, indexShift = 0) {
    if (list.empty) return -1

    def midIndex = list.size().intdiv(2)
    def indexValue = list[midIndex]

    if (indexValue == value) {
      midIndex + indexShift // forgot "+ indexShift"
    } else if (indexValue > value) {
      find(value, list[0..<midIndex], indexShift)
    } else {
      find(value, list[(midIndex + 1)..-1], indexShift + midIndex + 1) // off-by-one
    }
  }

  def find_(value, list) {
    def from = 0
    def to = list.size()

    while (to - from > 0) {
      def index = (from + to).intdiv(2) // used "/" instead of "intdiv"
      def valueAtIndex = list[index]

      if (valueAtIndex == value) {
        return index // didn't have return
      } else if (valueAtIndex > value) {
        to = index
      } else {
        from = index + 1
      }
    }

    return -1
  }
}
