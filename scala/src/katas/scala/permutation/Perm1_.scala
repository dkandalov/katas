package katas.scala.permutation

import org.junit.Test
import org.scalatest.junit.JUnitSuite

import scala.collection.mutable.ListBuffer

/**
 * User: DKandalov
 */
class Perm1_ extends JUnitSuite {
  @Test def shouldCreateListOfAllPermutations() {
    assert(permutation(ListBuffer()) === ListBuffer())
    assert(permutation(ListBuffer(1)) === ListBuffer(ListBuffer(1)))
    assert(permutation(ListBuffer(1, 2)) === ListBuffer(ListBuffer(1, 2), ListBuffer(2, 1)))
    assert(permutation(ListBuffer(1, 2, 3)) === ListBuffer(
      ListBuffer(1, 2, 3), ListBuffer(1, 3, 2), ListBuffer(2, 1, 3), ListBuffer(2, 3, 1), ListBuffer(3, 1, 2), ListBuffer(3, 2, 1)
    ))
    assert(permutation(ListBuffer(1, 2, 3, 4)) === ListBuffer(
      ListBuffer(1, 2, 3, 4), ListBuffer(1, 2, 4, 3), ListBuffer(1, 3, 2, 4), ListBuffer(1, 3, 4, 2), ListBuffer(1, 4, 2, 3), ListBuffer(1, 4, 3, 2),
      ListBuffer(2, 1, 3, 4), ListBuffer(2, 1, 4, 3), ListBuffer(2, 3, 1, 4), ListBuffer(2, 3, 4, 1), ListBuffer(2, 4, 1, 3), ListBuffer(2, 4, 3, 1),
      ListBuffer(3, 1, 2, 4), ListBuffer(3, 1, 4, 2), ListBuffer(3, 2, 1, 4), ListBuffer(3, 2, 4, 1), ListBuffer(3, 4, 1, 2), ListBuffer(3, 4, 2, 1),
      ListBuffer(4, 1, 2, 3), ListBuffer(4, 1, 3, 2), ListBuffer(4, 2, 1, 3), ListBuffer(4, 2, 3, 1), ListBuffer(4, 3, 1, 2), ListBuffer(4, 3, 2, 1)
    ))
  }

  def permutation(list: ListBuffer[Int]): ListBuffer[ListBuffer[Int]] = {
    if (list.isEmpty) return ListBuffer()
    if (list.size == 1) return ListBuffer(list)

    val result = ListBuffer[ListBuffer[Int]]()
    list.indices.foreach { i =>
      val subList = list.clone()
      subList.remove(i)
      permutation(subList).foreach { subResult =>
        subResult.insert(0, list(i))
        result += subResult
      }
    }
    result
  }
}