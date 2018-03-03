package katas.scala.sort.mergesort

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class MergeSort2_ extends AssertionsForJUnit
{
  @Test
  def shouldSortUsingMergeSort()
  {
    assert(sort(List()) === List())
    assert(sort(List(1)) === List(1))
    assert(sort(List(2, 1)) === List(1, 2))

    assert(sort(List(2, 3, 1)) === List(1, 2, 3))
    assert(sort(List(2, 1, 2)) === List(1, 2, 2))

    assert(sort(List(4, 1, 3, 2)) === List(1, 2, 3, 4))
  }

  def sort(list: List[Int]): List[Int] = list match {
    case List() => list
    case List(_) => list
    case _ =>
	    val parts = list.splitAt(list.size / 2)
	    merge(sort(parts._1), sort(parts._2))
  }

  def merge(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (List(), List()) => List()
    case (List(), l@_) => l
    case (l@_, List()) => l
    case _ =>
      if (list1.head < list2.head)
        list1.head :: merge(list1.tail, list2)
      else
        list2.head :: merge(list2.tail, list1)
  }
}