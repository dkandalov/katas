package katas.scala.pascaltriangle

import org.junit.Test
import org.scalatest.Matchers

/**
 * @author DKandalov
 */
class PascalTriangle1 extends Matchers {
  @Test def shouldComputeElementsOfPascalsTriangle() {
    pascal(0, -1) should equal(0)
    pascal(0, 0) should equal(1)
    pascal(0, 1) should equal(0)

    pascal(1, 0) should equal(1)
    pascal(1, 1) should equal(1)

    pascal(2, 0) should equal(1)
    pascal(2, 1) should equal(2)
    pascal(2, 2) should equal(1)

    pascal(3, 0) should equal(1)
    pascal(3, 1) should equal(3)
    pascal(3, 2) should equal(3)
    pascal(3, 3) should equal(1)
  }

  def pascal(depth: Int, pos: Int): Int = depth match {
    case 0 => if (pos == 0) 1 else 0
    case 1 => if (pos == 0 || pos == 1) 1 else 0
    case _ => pascal(depth - 1, pos - 1) + pascal(depth - 1, pos)
  }

  @Test def shouldComputePascalTriangle() {
    // TODO finish
    pascalTriangle(0) should equal(List())
    pascalTriangle(1) should equal(List(List(1)))
    pascalTriangle(2) should equal(List(
      List(1),
      List(1, 1)))
    pascalTriangle(3) should equal(List(
      List(1),
      List(1, 1),
      List(1, 2, 1)))
    pascalTriangle(4) should equal(List(
      List(1),
      List(1, 1),
      List(1, 2, 1),
      List(1, 3, 3, 1)))
    pascalTriangle(5) should equal(List(
      List(1),
      List(1, 1),
      List(1, 2, 1),
      List(1, 3, 3, 1),
      List(1, 4, 6, 4, 1)))
  }

  def pascalTriangle(depth: Int): List[List[Int]] = depth match {
    case 0 => List()
    case 1 => pascalTriangle(depth - 1) ::: List(List(1))
    case 2 => pascalTriangle(depth - 1) ::: List(List(1, 1))
    case _ => val listOfRows = pascalTriangle(depth - 1)
    listOfRows ::: List(computeNextRow(listOfRows.last)) // was confused with nested lists
  }

  def computeNextRow(list: List[Int]): List[Int] = 1 :: sumupList(list) ::: List(1) // spent some time figuring out that I could do this

  def sumupList(list: List[Int]): List[Int] = list match {
    case List(a, b) => List(a + b)
    case _ => (list.head + list.tail.head) :: sumupList(list.tail)
  }
}