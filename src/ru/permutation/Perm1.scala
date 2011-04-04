package ru.permutation

import collection.mutable.ListBuffer
import org.junit.Assert._
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit, ShouldMatchersForJUnit}
import org.junit.{Ignore, Test, Before}
import collection.Seq

/**
 * User: DKandalov
 */
class Perm1 extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def shouldCreateListOfAllPermutations() {
    assert(permutation(ListBuffer()) === ListBuffer(ListBuffer()))
    assert(permutation(ListBuffer(1)) === ListBuffer(ListBuffer(1)))
    assert(permutation(ListBuffer(1, 2)) === ListBuffer(ListBuffer(1, 2), ListBuffer(2, 1)))
    assert(permutation(ListBuffer(1, 2, 3)) === ListBuffer(
      ListBuffer(1, 2, 3), ListBuffer(2, 1, 3), ListBuffer(2, 3, 1),
      ListBuffer(1, 3, 2), ListBuffer(3, 1, 2), ListBuffer(3, 2, 1)
    ))
  }

  def permutation(list: ListBuffer[Int]): ListBuffer[ListBuffer[Int]] =
  {
    if (list.size == 0) return ListBuffer(ListBuffer())
    if (list.size == 1) return ListBuffer(list)

    var result = ListBuffer[ListBuffer[Int]]()

    0.until(fact(list.size)).foreach { // used 1.until() instead of 0.until()
      i: Int =>
        result = result + (ListBuffer() ++ list) // didn't copy list into new listBuffer
        swapRight(list, i % list.size)
    }

    result
  }

  @Test def shouldSwapValueInList() {
    assert(swapRight(ListBuffer(1, 2), 0) == ListBuffer(2, 1))
    assert(swapRight(ListBuffer(1, 2), 1) == ListBuffer(2, 1))
    assert(swapRight(ListBuffer(1, 2, 3), 2) == ListBuffer(3, 2, 1))
    assert(swapRight(ListBuffer(3, 2, 1), 0) == ListBuffer(2, 3, 1))
    assert(swapRight(ListBuffer(2, 3, 1), 1) == ListBuffer(2, 1, 3))
  }

  def swapRight(list: ListBuffer[Int], position: Int): ListBuffer[Int] =
  {
    val nextPosition = (position + 1) % list.size
    val tmp = list(position)
    list(position) = list(nextPosition)
    list(nextPosition) = tmp
    list
  }

  @Test def shouldCalculateFactorial() {
    assertEquals(fact(0), 1)
    assertEquals(fact(1), 1)
    assertEquals(fact(2), 2)
    assertEquals(fact(3), 6)
    assertEquals(fact(4), 24)
  }

  def fact(i: Int): Int =
  {
    if (i < 0) return -1
    if (i == 0) return 1
    if (i < 3) return i
    fact(i - 1) * i
  }

}