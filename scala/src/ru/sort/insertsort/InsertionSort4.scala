package ru.sort.insertsort

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
;

/*
 * User: dima
 * Date: 22/3/11
 * Time: 10:35 PM
 */
class InsertionSort4 extends AssertionsForJUnit {
  @Test
  def shouldSortList() {
    assert(sort(List()) === List())
    assert(sort(List(1)) === List(1))
    assert(sort(List(1, 2)) === List(1, 2))
    assert(sort(List(2, 1)) === List(1, 2))
    assert(sort(List(2, 3, 1)) === List(1, 2, 3))
  }

  def sort(values: List[Int]): List[Int] = {
    sort(List(), values)
  }

  def sort(sortedValues: List[Int], values: List[Int]): List[Int] = {
    values match {
      case List() => sortedValues // returned empty list instead of sortedValues
      case List(x, _*) => sort(insert(sortedValues, values.head), values.tail) // didn't pass sorted list into sort() call
    }
  }

  def insert(sortedValues: List[Int], value: Int): List[Int] =
    sortedValues match {
      case List() => List(value)
      case List(x, _*) =>
        if (value <= x)
          value :: sortedValues
        else
          x :: insert(sortedValues.tail, value)
    }
}