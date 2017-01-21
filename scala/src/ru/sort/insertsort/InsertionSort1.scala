package ru.sort.insertsort

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/*
 * User: dima
 * Date: 15/2/11
 * Time: 7:25 PM
 */
class InsertionSort1 extends AssertionsForJUnit {
  @Test def shouldSortLists() {
    assert(sort(List()) === List())
    assert(sort(List(1)) === List(1))
    assert(sort(List(2, 1)) === List(1, 2))
    assert(sort(List(2, 3, 1)) === List(1, 2, 3))
    assert(sort(List(2, 2, 1)) === List(1, 2, 2))
  }

  @Test def shouldInsertValueIntoList() {
    assert(insert(1, List()) === List(1))

    assert(insert(1, List(2)) === List(1, 2))
    assert(insert(1, List(1)) === List(1, 1))
    assert(insert(2, List(1)) === List(1, 2))

    assert(insert(1, List(2, 3)) === List(1, 2, 3))
    assert(insert(2, List(1, 3)) === List(1, 2, 3))
    assert(insert(3, List(1, 2)) === List(1, 2, 3))
  }

  def sort(list: List[Int]): List[Int] = {
    list match {
      case List() => List()
      case x :: xs => insert(x, sort(xs)) // used insert(x, xs) instead of insert(x, sort(xs))
    }
  }

  def insert(i: Int, list: List[Int]): List[Int] = {
    list match {
      case List() => List(i)
      case x :: xs => if (i <= x) i :: list else x :: insert(i, xs)
    }
  }
}
