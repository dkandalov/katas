package katas.scala.sort.mergesort

import org.junit.Test
import org.scalatest.Matchers


class MergeSort12 extends Matchers {
	@Test def sortValues() {
		sort(Seq()) should equal(Seq())
		sort(Seq(1)) should equal(Seq(1))
		sort(Seq(1, 2)) should equal(Seq(1, 2))
		sort(Seq(2, 1)) should equal(Seq(1, 2))
		sort(Seq(1, 2, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(1, 3, 2)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 1, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 3, 1)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 2, 1)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 1, 2)) should equal(Seq(1, 2, 3))
	}

	def sort[T](values: Seq[T])(implicit ordered: T => Ordered[T]): Seq[T] = {
		if (values.length <= 1) return values
		val parts = values.splitAt(values.length / 2)
		merge(sort(parts._1), sort(parts._2))
	}

	def merge[T](part1: Seq[T], part2: Seq[T])(implicit ordered: T => Ordered[T]): Seq[T] = {
		if (part1.isEmpty) part2
		else if (part2.isEmpty) part1
		else if (part1.head < part2.head) part1.head +: merge(part1.tail, part2)
		else part2.head +: merge(part1, part2.tail)
	}
}