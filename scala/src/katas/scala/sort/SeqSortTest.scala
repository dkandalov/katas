package katas.scala.sort

import org.junit.Test
import org.scalatest.Matchers
import org.specs2.matcher.ShouldMatchers

import scala.reflect.ClassTag

trait SeqSortTest extends ShouldMatchers {

	def sort[T](seq: Seq[T])(implicit ordered: (T => Ordered[T]), tag: ClassTag[T]): Seq[T]

	@Test def shouldSortIntegerSequencesOfDifferentSize() {
		sort(Seq()) should equalTo(Seq())
		sort(Seq(1)) should equalTo(Seq(1))
		sort(Seq(1, 2)) should equalTo(Seq(1, 2))
		sort(Seq(2, 1)) should equalTo(Seq(1, 2))

		sort(Seq(1, 2, 2)) should equalTo(Seq(1, 2, 2))
		sort(Seq(2, 2, 1)) should equalTo(Seq(1, 2, 2))

		sort(Seq(1, 2, 3)) should equalTo(Seq(1, 2, 3))
		sort(Seq(1, 3, 2)) should equalTo(Seq(1, 2, 3))
		sort(Seq(2, 1, 3)) should equalTo(Seq(1, 2, 3))
		sort(Seq(2, 3, 1)) should equalTo(Seq(1, 2, 3))
		sort(Seq(3, 1, 2)) should equalTo(Seq(1, 2, 3))
		sort(Seq(3, 2, 1)) should equalTo(Seq(1, 2, 3))

		val seqWithDuplicates = Range(1, 4).flatMap{ n => Array.fill(n){n}.toSeq }
		Matchers.every(seqWithDuplicates.permutations.toSeq.map(sort(_))) should equalTo(seqWithDuplicates)

		(1 to 8).map(Range(1, _).toSeq).foreach { seq =>
			Matchers.every(seq.permutations.toSeq.map(sort(_))) should equalTo(seq)
		}
	}
}