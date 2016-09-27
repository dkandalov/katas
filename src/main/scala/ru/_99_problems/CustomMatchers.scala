package ru._99_problems

import org.scalatest.matchers.{BeMatcher, HavePropertyMatchResult, HavePropertyMatcher, MatchResult}


trait CustomMatchers {
	val noDuplicates = new NoDuplicates()

	class NoDuplicates extends HavePropertyMatcher[Seq[Any], Any] {
		def apply(seq: Seq[Any]) = {
			val hasDuplicates = seq.toSet.size == seq.size
			HavePropertyMatchResult(
				hasDuplicates,
				"has no duplicates",
			  "true",
				hasDuplicates
			)
		}

	}

	def subSetOf[T](seq: Seq[T]): SubSetOf[T] = new SubSetOf(seq)

	class SubSetOf[T](seq: Seq[T]) extends BeMatcher[Seq[T]] {
		def apply(left: Seq[T]) =
			MatchResult(
				left.forall{seq.contains(_)},
				left.toString + " was not subset of expected values",
				left.toString + " was subset of expected values"
			)
	}


	def oneOf[T](acceptedValues: T*): OneOfMatcher[T] = new OneOfMatcher(acceptedValues.toSeq)
	def oneOf[T](i: Iterator[T]): OneOfMatcher[T] = new OneOfMatcher(i.toSeq)

	class OneOfMatcher[T](values: Seq[T]) extends BeMatcher[T] {
		def apply(left: T) =
			MatchResult(
				values.contains(left),
				left.toString + " was not one of expected values",
				left.toString + " was one of expected values"
			)
	}
}

object CustomMatchers extends CustomMatchers