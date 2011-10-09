package ru.permutation

import org.junit.Test
import org.scalatest.matchers.ShouldMatchers

/**
 * @author DKandalov
 */

class Perm3 extends ShouldMatchers {

  @Test def shouldFindPermutationsOfAList() {
    perm(List()) should equal(List())
    perm(List(1)) should equal(List(List(1)))
    perm(List(1, 2)) should equal(List(List(1, 2), List(2, 1))) // mistake in test
    perm(List(1, 2, 3)) should equal(List(
      List(1, 2, 3), List(2, 1, 3), List(2, 3, 1),
      List(1, 3, 2), List(3, 1, 2), List(3, 2, 1)
    ))
    perm(List(1, 2, 3, 4)) should equal(List(
      List(1, 2, 3, 4), List(2, 1, 3, 4), List(2, 3, 1, 4), List(2, 3, 4, 1),
      List(1, 3, 4, 2), List(3, 1, 4, 2), List(3, 4, 1, 2), List(3, 4, 2, 1),
      List(1, 4, 2, 3), List(4, 1, 2, 3), List(4, 2, 1, 3), List(4, 2, 3, 1),
      List(1, 2, 3, 4), List(2, 1, 3, 4), List(2, 3, 1, 4), List(2, 3, 4, 1),
      List(1, 3, 4, 2), List(3, 1, 4, 2), List(3, 4, 1, 2), List(3, 4, 2, 1),
      List(1, 4, 2, 3), List(4, 1, 2, 3), List(4, 2, 1, 3), List(4, 2, 3, 1)
    ))
  }

  def perm(list: List[Int]): List[List[Int]] = {
    var listCopy = list
    0.until(fact(list.size)).foldLeft(List[List[Int]]()) { (acc, i) =>
      val result = acc ::: List(listCopy)
      listCopy = swapElements(i, listCopy)
      result
    }
  }

  def swapElements(i: Int, list: List[Int]): List[Int] = {
    val p1: Int = i % list.size
    val p2: Int = (i + 1) % list.size
    val tmp = list(p1)
    list.updated(p1, list(p2)).updated(p2, tmp)
  }

  @Test def shouldCalculateFactorial() {
    List(1, 2, 3, 4, 5, 6).map(fact) should equal(List(1, 2, 6, 24, 120, 720))
  }

  def fact(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException()
    if (n <= 2) return n
    n * fact(n - 1)
  }
}