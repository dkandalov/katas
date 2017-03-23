package katas.scala.bsearch

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
;

/*
 * User: dima
 * Date: 20/2/11
 * Time: 8:14 AM
 */
class BSearch7 extends AssertionsForJUnit {
  @Test def shouldFindElementInList() {
    assert(search(1, List()) === None)

    assert(search(0, List(1)) === None)
    assert(search(1, List(1)) === Some(0))
    assert(search(2, List(1)) === None)

    assert(search(0, List(1, 2)) === None)
    assert(search(1, List(1, 2)) === Some(0))
    assert(search(2, List(1, 2)) === Some(1))
    assert(search(3, List(1, 2)) === None)

    assert(search(0, List(1, 2, 3)) === None)
    assert(search(1, List(1, 2, 3)) === Some(0))
    assert(search(2, List(1, 2, 3)) === Some(1))
    assert(search(3, List(1, 2, 3)) === Some(2))
    assert(search(4, List(1, 2, 3)) === None)

    assert(search(0, List(1, 2, 3, 4)) === None)
    assert(search(1, List(1, 2, 3, 4)) === Some(0))
    assert(search(2, List(1, 2, 3, 4)) === Some(1))
    assert(search(3, List(1, 2, 3, 4)) === Some(2))
    assert(search(4, List(1, 2, 3, 4)) === Some(3))
    assert(search(5, List(1, 2, 3, 4)) === None)
  }

  def search(value: Int, list: List[Int]): Option[Int] = {
    if (list.size == 0) return None

    val midPos: Int = list.size / 2
    if (list(midPos) == value) {
      Some(midPos)
    } else if (list(midPos) > value) {
      search(value, list.splitAt(midPos)._1) match {
        case None => None
        case Some(i) => Some(i)
      }
    } else if (list(midPos) < value) {
      search(value, list.splitAt(midPos + 1)._2) match {
        case None => None
        case Some(i) => Some(midPos + 1 + i) // off-by-one mistake... midPost + 1
      }
    } else {
      None
    }
  }
}