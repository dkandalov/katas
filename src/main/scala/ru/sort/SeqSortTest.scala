package ru.sort

import org.scalatest.Matchers
import org.junit.Test
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.{MatchFailed, MatchResult, Matcher}
import org.specs2.matcher.MatchFailureException
import org.scalatest.enablers.Collecting._

import scala.reflect.ClassTag

trait SeqSortTest extends Matchers {

	def sort[T](seq: Seq[T])(implicit ordered: (T => Ordered[T]), tag: ClassTag[T]): Seq[T]

	@Test def shouldSortIntegerSequencesOfDifferentSize() {
		sort(Seq()) should equal(Seq())
		sort(Seq(1)) should equal(Seq(1))
		sort(Seq(1, 2)) should equal(Seq(1, 2))
		sort(Seq(2, 1)) should equal(Seq(1, 2))

		sort(Seq(1, 2, 2)) should equal(Seq(1, 2, 2))
		sort(Seq(2, 2, 1)) should equal(Seq(1, 2, 2))

		sort(Seq(1, 2, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(1, 3, 2)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 1, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 3, 1)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 1, 2)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 2, 1)) should equal(Seq(1, 2, 3))

		val seq: Seq[Int] = Range(1, 4).flatMap { n => Array.fill(n){n}.toSeq }
		all(seq.permutations)(collectingNatureOfGenTraversable[Seq[Int], Iterator]) should equal(seq)
//		allPermutationsShouldBeEqual(seq)

		(1 to 8).map(Range(1, _).toSeq).foreach { seq =>
				seq.permutations.foreach { perm =>
						sort(perm) should equal(seq)
				}
		}
	}

	private def allPermutationsShouldBeEqual[T](seq: Seq[T]): Unit = {
		seq.permutations.foreach{ it =>
			val matchResult = equal(seq).matcher[Seq[T]].apply(it)
			matchResult match {
				case MatchFailed(failureMessage) => throw new TestFailedException(failureMessage, 0)
				case _ =>
			}
		}
//		new Matcher[Seq[T]] {
//			override def apply(left: Seq[T]): MatchResult = {
//				new MatchResult(false, "", "")
//			}
//		}
	}
}