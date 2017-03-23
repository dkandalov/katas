package katas.scala.bsearch

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 * User: DKandalov
 */

class BSearch2_ extends AssertionsForJUnit {
  @Test def shouldFindItemPosition() {
    assert(find(0, List()) === -1)

    assert(find(0, List(1)) === -1)
    assert(find(1, List(1)) === 0)
    assert(find(2, List(1)) === -1)

    assert(find(0, List(1, 2)) === -1)
    assert(find(1, List(1, 2)) === 0)
    assert(find(2, List(1, 2)) === 1)
    assert(find(3, List(1, 2)) === -1)

    assert(find(0, List(1, 2, 3)) === -1)
    assert(find(1, List(1, 2, 3)) === 0)
    assert(find(2, List(1, 2, 3)) === 1)
    assert(find(3, List(1, 2, 3)) === 2)
    assert(find(4, List(1, 2, 3)) === -1)

    assert(find(0, List(1, 2, 3, 4)) === -1)
    assert(find(1, List(1, 2, 3, 4)) === 0)
    assert(find(2, List(1, 2, 3, 4)) === 1)
    assert(find(3, List(1, 2, 3, 4)) === 2)
    assert(find(4, List(1, 2, 3, 4)) === 3)
    assert(find(5, List(1, 2, 3, 4)) === -1)
  }

  def find(v : Int, values : List[Int], shift : Int = 0) : Int = {
    values match {
      case List() => -1
      case List(x) => if (v == x) shift else -1
      case List(_*) =>
        val midPos = values.size / 2
        val midValue = values(midPos)
        if (v == midValue) {
          shift + midPos
        } else if (v < midValue) {
          find(v, values.splitAt(values.size / 2)._1, shift) // had "values.size / 2 - 1"
        } else {
          find(v, values.splitAt((values.size / 2) + 1)._2, shift + midPos + 1) // didn't add +1 to shift + midPos
        }
    }
  }
}