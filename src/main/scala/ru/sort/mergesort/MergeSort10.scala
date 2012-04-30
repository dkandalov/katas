package ru.sort.mergesort

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import ru.util.Pomodoro

/**
 * User: dima
 * Date: 30/04/2012
 */
@Pomodoro("1")
class MergeSort10 extends ShouldMatchers {
  @Test def shouldSortList() {
    sort(List()) should equal(List())
    sort(List(1)) should equal(List(1))
    sort(List(1, 2)) should equal(List(1, 2))
    sort(List(2, 1)) should equal(List(1, 2))
    sort(List(2, 3, 1)) should equal(List(1, 2, 3))

	  List(1, 2, 3, 4).permutations.foreach { list =>
			  sort(list) should equal(List(1, 2, 3, 4))
	  }
  }

  def sort(list: List[Int]): List[Int] = {
    if (list.size <= 1) return list
    val (part1, part2) = list.splitAt(list.size / 2)
    merge(sort(part1), sort(part2))
  }

  def merge(list1: List[Int], list2: List[Int]): List[Int] = {
    if (list1.isEmpty) return list2
    if (list2.isEmpty) return list1
    if (list1(0) < list2(0))
      list1(0) :: merge(list1.tail, list2)
    else
      list2(0) :: merge(list1, list2.tail)
  }
}