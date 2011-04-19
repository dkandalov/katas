package ru.sort.mergesort

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import ru.permutation.Perm1
import collection.mutable.ListBuffer

class MergeSort3 extends AssertionsForJUnit {
  @Test
  def shouldSortUsingMergeSort() {
    assert(sort(List()) === List())
    assert(sort(List(1)) === List(1))
    assert(sort(List(1, 1)) === List(1, 1))
    assert(sort(List(1, 2)) === List(1, 2))
    assert(sort(List(2, 1)) === List(1, 2)) // was incorrect test
    assert(sort(List(3, 1, 2)) === List(1, 2, 3))

    new Perm1().permutation(ListBuffer(1, 2, 3, 4, 5)).foreach { listBuffer =>
        assert(sort(listBuffer.toList) == List(1, 2, 3, 4, 5))
    }
  }

  def sort(list: List[Int]): List[Int] =
  {
    if (list.size <= 1) return list // forgot to terminate for lists with size == 1, caused stack overflow
    val (left, right) = list.splitAt(list.length / 2)
    merge(sort(left), sort(right))
  }

  def merge(left: List[Int], right: List[Int]): List[Int] =
  {
    (left, right) match {
      case (List(), List()) => List()
      case (List(), List(_*)) => right
      case (List(_*), List()) => left
      case (List(x1, _*), List(x2, _*)) =>
        if (x1 > x2) x2 :: merge(left, right.tail) else x1 :: merge(left.tail, right)
    }
  }
}