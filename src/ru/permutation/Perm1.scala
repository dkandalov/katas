package ru.permutation

import collection.mutable.ListBuffer
import org.junit.{Test, Before}
import org.junit.Assert._
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit, ShouldMatchersForJUnit}

/**
 * User: DKandalov
 */
class Perm1 extends JUnitSuite with ShouldMatchersForJUnit
{
  @Test def shouldCreateListOfAllPermutations() // TODO
  {
    assert(perm(Array()).toList === Array(Array()).toList)
//    assertEquals(perm(Array(1)), Array(Array(1)))
//    assert(perm(Array(1, 2)) === Array(Array(1, 2), Array(2, 1)))
//    println(perm(Array()).toString)
//    println perm(Array(1)).toString
//    println perm(Array(1,2)).toString
    // 123, 312, 132
  }

  def perm(list: Array[Int]): Array[Array[Int]] =
  {
    if (list.size == 0) return Array(Array())
    if (list.size == 1) return Array(list)


    var result = new Array[Array[Int]](0)

    1.to(list.size).foreach { i :Int =>
      val split = list.splitAt(i)
//      var subResult = new Array[Any](0)
      perm(split._1 ++ split._2.tail).foreach { a : Array[Int] =>
        result = result ++ Array(Array(split._2.head) ++ a)
      }
    }

    result
  }

/*
  @Test def aaa()
  {
    assertArrayEquals(Array(2, 1), swapRight(Array(1, 2), 1))
    assertArrayEquals(Array(3, 1, 2), swapRight(Array(1, 2, 3), 2))
    assertArrayEquals(Array(1, 3, 2), swapRight(Array(3, 1, 2), 2))
  }
*/

  def swapRight(list: Array[Int], position: Int): Array[Int] =
  {
    var nextPosition = position + 1
    if (nextPosition >= list.size) nextPosition -= list.size

    Array()
  }

  @Test def shouldCalculateFactorial()
  {
    assertEquals(fact(0), 0)
    assertEquals(fact(1), 1)
    assertEquals(fact(2), 2)
    assertEquals(fact(3), 6)
    assertEquals(fact(4), 24)
  }

  def fact(i: Int): Int =
  {
    if (i < 0) return -1
    if (i < 3) return i
    fact(i - 1) * i
  }

}