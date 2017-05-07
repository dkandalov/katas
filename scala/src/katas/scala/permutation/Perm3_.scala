package katas.scala.permutation

import org.junit.Test
import org.scalatest.Matchers

/**
 * @author DKandalov
 */
class Perm3_ extends Matchers {

  @Test def shouldFindPermutationsOfAList() {
    perm(List()) should equal(List())
    perm(List(1)) should equal(List(List(1)))
    perm(List(1, 2)) should equal(List(List(1, 2), List(2, 1)))
    perm(List(1, 2, 3)) should equal(List(
      List(1, 2, 3), List(1, 3, 2),
      List(2, 1, 3), List(2, 3, 1),
      List(3, 1, 2), List(3, 2, 1)
    ))
    perm(List(1, 2, 3, 4)) should equal(List(
      List(1, 2, 3, 4), List(1, 2, 4, 3), List(1, 3, 2, 4), List(1, 3, 4, 2), List(1, 4, 2, 3), List(1, 4, 3, 2),
      List(2, 1, 3, 4), List(2, 1, 4, 3), List(2, 3, 1, 4), List(2, 3, 4, 1), List(2, 4, 1, 3), List(2, 4, 3, 1),
      List(3, 1, 2, 4), List(3, 1, 4, 2), List(3, 2, 1, 4), List(3, 2, 4, 1), List(3, 4, 1, 2), List(3, 4, 2, 1),
      List(4, 1, 2, 3), List(4, 1, 3, 2), List(4, 2, 1, 3), List(4, 2, 3, 1), List(4, 3, 1, 2), List(4, 3, 2, 1)
    ))
    perm(List(1, 2, 3, 4, 5)) should equal(List(
      List(1, 2, 3, 4, 5), List(1, 2, 3, 5, 4), List(1, 2, 4, 3, 5), List(1, 2, 4, 5, 3), List(1, 2, 5, 3, 4), List(1, 2, 5, 4, 3), List(1, 3, 2, 4, 5), List(1, 3, 2, 5, 4), List(1, 3, 4, 2, 5), List(1, 3, 4, 5, 2), List(1, 3, 5, 2, 4), List(1, 3, 5, 4, 2), List(1, 4, 2, 3, 5), List(1, 4, 2, 5, 3), List(1, 4, 3, 2, 5), List(1, 4, 3, 5, 2), List(1, 4, 5, 2, 3), List(1, 4, 5, 3, 2), List(1, 5, 2, 3, 4), List(1, 5, 2, 4, 3), List(1, 5, 3, 2, 4), List(1, 5, 3, 4, 2), List(1, 5, 4, 2, 3), List(1, 5, 4, 3, 2),
      List(2, 1, 3, 4, 5), List(2, 1, 3, 5, 4), List(2, 1, 4, 3, 5), List(2, 1, 4, 5, 3), List(2, 1, 5, 3, 4), List(2, 1, 5, 4, 3), List(2, 3, 1, 4, 5), List(2, 3, 1, 5, 4), List(2, 3, 4, 1, 5), List(2, 3, 4, 5, 1), List(2, 3, 5, 1, 4), List(2, 3, 5, 4, 1), List(2, 4, 1, 3, 5), List(2, 4, 1, 5, 3), List(2, 4, 3, 1, 5), List(2, 4, 3, 5, 1), List(2, 4, 5, 1, 3), List(2, 4, 5, 3, 1), List(2, 5, 1, 3, 4), List(2, 5, 1, 4, 3), List(2, 5, 3, 1, 4), List(2, 5, 3, 4, 1), List(2, 5, 4, 1, 3), List(2, 5, 4, 3, 1),
      List(3, 1, 2, 4, 5), List(3, 1, 2, 5, 4), List(3, 1, 4, 2, 5), List(3, 1, 4, 5, 2), List(3, 1, 5, 2, 4), List(3, 1, 5, 4, 2), List(3, 2, 1, 4, 5), List(3, 2, 1, 5, 4), List(3, 2, 4, 1, 5), List(3, 2, 4, 5, 1), List(3, 2, 5, 1, 4), List(3, 2, 5, 4, 1), List(3, 4, 1, 2, 5), List(3, 4, 1, 5, 2), List(3, 4, 2, 1, 5), List(3, 4, 2, 5, 1), List(3, 4, 5, 1, 2), List(3, 4, 5, 2, 1), List(3, 5, 1, 2, 4), List(3, 5, 1, 4, 2), List(3, 5, 2, 1, 4), List(3, 5, 2, 4, 1), List(3, 5, 4, 1, 2), List(3, 5, 4, 2, 1),
      List(4, 1, 2, 3, 5), List(4, 1, 2, 5, 3), List(4, 1, 3, 2, 5), List(4, 1, 3, 5, 2), List(4, 1, 5, 2, 3), List(4, 1, 5, 3, 2), List(4, 2, 1, 3, 5), List(4, 2, 1, 5, 3), List(4, 2, 3, 1, 5), List(4, 2, 3, 5, 1), List(4, 2, 5, 1, 3), List(4, 2, 5, 3, 1), List(4, 3, 1, 2, 5), List(4, 3, 1, 5, 2), List(4, 3, 2, 1, 5), List(4, 3, 2, 5, 1), List(4, 3, 5, 1, 2), List(4, 3, 5, 2, 1), List(4, 5, 1, 2, 3), List(4, 5, 1, 3, 2), List(4, 5, 2, 1, 3), List(4, 5, 2, 3, 1), List(4, 5, 3, 1, 2), List(4, 5, 3, 2, 1),
      List(5, 1, 2, 3, 4), List(5, 1, 2, 4, 3), List(5, 1, 3, 2, 4), List(5, 1, 3, 4, 2), List(5, 1, 4, 2, 3), List(5, 1, 4, 3, 2), List(5, 2, 1, 3, 4), List(5, 2, 1, 4, 3), List(5, 2, 3, 1, 4), List(5, 2, 3, 4, 1), List(5, 2, 4, 1, 3), List(5, 2, 4, 3, 1), List(5, 3, 1, 2, 4), List(5, 3, 1, 4, 2), List(5, 3, 2, 1, 4), List(5, 3, 2, 4, 1), List(5, 3, 4, 1, 2), List(5, 3, 4, 2, 1), List(5, 4, 1, 2, 3), List(5, 4, 1, 3, 2), List(5, 4, 2, 1, 3), List(5, 4, 2, 3, 1), List(5, 4, 3, 1, 2), List(5, 4, 3, 2, 1)
    ))
  }

  def perm(list: List[Int]): List[List[Int]] = {
    if (list.isEmpty) return List()
    if (list.size == 1) return List(list)

    var result = List[List[Int]]()
    0.until(list.size).foreach { (i: Int) =>
      val subList = list.take(i) ::: list.drop(i + 1)
      perm(subList).foreach { (subResult: List[Int]) =>
        result = result ::: List(list(i) :: subResult)
      }
    }
    result
  }
}