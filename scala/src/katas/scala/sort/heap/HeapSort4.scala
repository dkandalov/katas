package katas.scala.sort.heap

import katas.scala.permutation.Perm3_
import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.Matchers._
import org.specs2.matcher.ShouldMatchers

/**
 * User: dima
 * Date: 05/11/2011
 */
class HeapSort4 extends ShouldMatchers {
  @Test def shouldSortList() {
    sort(List()) should equalTo(List())
    sort(List(1)) should equalTo(List(1))
    sort(List(1, 2)) should equalTo(List(1, 2))
    sort(List(2, 1)) should equalTo(List(1, 2))
    sort(List(3, 1, 2)) should equalTo(List(1, 2, 3))

    new Perm3_().perm(List(1, 2, 3, 4, 5)).foreach { list =>
      sort(list) should equalTo(List(1, 2, 3, 4, 5))
    }
  }

  def sort(list: List[Int]): List[Int] = {
    collectMaxValuesFrom(list.foldRight[AHeap](EmptyHeap()) { (i, heap) => heap.add(i) })
  }

  private def collectMaxValuesFrom(heap: AHeap): List[Int] = heap match {
    case EmptyHeap() => List()
    case default =>
      val result = heap.removeMax()
      val maxValue = result._1
      val newHeap = result._2
      collectMaxValuesFrom(newHeap) ::: List(maxValue)
  }

  @Test def shouldAddElementsToEmptyHeap() {
    EmptyHeap() should equalTo(EmptyHeap())
    Heap(1) should equalTo(Heap(1))

    EmptyHeap().add(4) should equalTo(Heap(4))
    EmptyHeap().add(4).add(3) should equalTo(Heap(4, Heap(3)))
    EmptyHeap().add(4).add(3).add(2) should equalTo(Heap(4, Heap(3), Heap(2)))
    EmptyHeap().add(4).add(2).add(3) should equalTo(Heap(4, Heap(2), Heap(3)))
    EmptyHeap().add(4).add(2).add(3).add(1) should equalTo(Heap(4, Heap(2, Heap(1)), Heap(3)))

    EmptyHeap().add(4).add(5) should equalTo(Heap(5, Heap(4)))
    EmptyHeap().add(4).add(5).add(3) should equalTo(Heap(5, Heap(4), Heap(3)))

    EmptyHeap().add(4).add(4) should equalTo(Heap(4, Heap(4)))
    EmptyHeap().add(4).add(4).add(4) should equalTo(Heap(4, Heap(4), Heap(4)))
  }

  @Test def shouldRemoveMaxElementFromHeap() {
    intercept[UnsupportedOperationException] { EmptyHeap().removeMax() }
    Heap(3).removeMax() should equalTo((3, EmptyHeap()))
    Heap(3, Heap(2)).removeMax() should equalTo((3, Heap(2)))
    Heap(3, Heap(2), Heap(1)).removeMax() should equalTo((3, Heap(2, EmptyHeap(), Heap(1))))
    Heap(3, Heap(1), Heap(2)).removeMax() should equalTo((3, Heap(2, Heap(1))))
    Heap(3, Heap(1, Heap(0)), Heap(2)).removeMax() should equalTo((3, Heap(2, Heap(1, Heap(0)))))
  }

  abstract class AHeap {
    def value(): Int
    def removeMax(): (Int, AHeap)
    def add(newValue: Int): AHeap
    def sink(): AHeap
  }

  case class EmptyHeap() extends AHeap {
    override def add(newValue: Int) = Heap(newValue)
    override def removeMax() = throw new UnsupportedOperationException
    override def value() = Int.MinValue
    override def sink(): EmptyHeap = this
  }

  case class Heap(value: Int, left: AHeap = EmptyHeap(), right: AHeap = EmptyHeap()) extends AHeap {

    override def removeMax(): (Int, AHeap) = (value, sink())

    override
    def sink(): AHeap = {
      if (left == EmptyHeap() && right == EmptyHeap()) EmptyHeap()
      else if (left.value > right.value) Heap(left.value(), left.sink(), right)
      else Heap(right.value(), left, right.sink())
    }

    override
    def add(newValue: Int): Heap = {
      if (newValue > value) Heap(newValue, this) // this is a random choice to add current root as left child
      else if (left.value > right.value) Heap(value, left, right.add(newValue))
      else Heap(value, left.add(newValue), right)
    }
  }
}