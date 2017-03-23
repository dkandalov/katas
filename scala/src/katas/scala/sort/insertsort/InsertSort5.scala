package katas.scala.sort.insertsort

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/**
 * @author DKandalov
 */
class InsertSort5 extends AssertionsForJUnit {

  @Test def shouldSortList() {
    assert(sort(List()) === List())
    assert(sort(List(1)) === List(1))
    assert(sort(List(1, 2)) === List(1, 2))
    assert(sort(List(2, 1)) === List(1, 2))
    assert(sort(List(2, 3, 1)) === List(1, 2, 3))
    assert(sort(List(1, 2, 3)) === List(1, 2, 3))
  }

  private def sort(list: List[Int], sortedPart: List[Int] = List()): List[Int] =
    list match {
      case List() => sortedPart.reverse // forgot to reverse
      case x :: xs => sort(xs, insert(x, sortedPart)) // got this part wrong the first time
    }

  def insert(value: Int, list: List[Int]): List[Int] =
    list match {
      case List() => List(value)
      case x :: xs => if (value < x) x :: insert(value, xs) else value :: list
    }
}