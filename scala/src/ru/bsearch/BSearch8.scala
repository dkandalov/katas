package ru.bsearch

import org.scalatest.Matchers
import org.junit.Test

/**
 * User: dima
 * Date: 17/11/2011
 */
class BSearch8 extends Matchers {
  @Test def shouldFindIndexOfItemsInAList() {
    search(0, List()) should equal(-1)

    search(0, List(1)) should equal(-1)
    search(1, List(1)) should equal(0)
    search(2, List(1)) should equal(-1)

    search(0, List(1, 2)) should equal(-1)
    search(1, List(1, 2)) should equal(0)
    search(2, List(1, 2)) should equal(1)
    search(3, List(1, 2)) should equal(-1)

    search(0, List(1, 2, 3)) should equal(-1)
    search(1, List(1, 2, 3)) should equal(0)
    search(2, List(1, 2, 3)) should equal(1)
    search(3, List(1, 2, 3)) should equal(2)
    search(4, List(1, 2, 3)) should equal(-1)

    search(0, List(1, 2, 3, 4)) should equal(-1)
    search(1, List(1, 2, 3, 4)) should equal(0)
    search(2, List(1, 2, 3, 4)) should equal(1)
    search(3, List(1, 2, 3, 4)) should equal(2)
    search(4, List(1, 2, 3, 4)) should equal(3)
    search(5, List(1, 2, 3, 4)) should equal(-1)
  }

  def search(n: Int, list: List[Int], shift: Int = 0): Int = {
    if (list.isEmpty) return -1 // didn't think about empty lists

    val midPos = list.size / 2
    val midValue = list(midPos)
    if (n < midValue) {
      search(n, list.dropRight(list.size - midPos), shift) // wasn't sure about "- midPos"
    } else if (n > midValue) {
      search(n, list.drop(midPos + 1), shift + midPos + 1) // didn't add "midPos + 1"
    } else { // if (n == midValue)
      midPos + shift
    }
  }
}