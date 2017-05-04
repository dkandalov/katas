package _99_problems

import org.specs2.matcher.{Expectable, MatchResult, Matcher}


trait CustomMatchers {
	def noDuplicates[T]: Matcher[Seq[T]] = new NoDuplicates[T]()

	class NoDuplicates[T]() extends Matcher[Seq[T]] {
		def apply[S <: Seq[T]](value: Expectable[S]): MatchResult[S] = {
			result(
				value.value.toSet.size == value.value.size,
				value.description + " is None",
				value.description + " is not None",
				value
			)
		}
	}

	def subSetOf[T](seq: Seq[T]): SubSetOf[T] = new SubSetOf(seq)

	class SubSetOf[T](seq: Seq[T]) extends Matcher[Seq[T]] {
		override def apply[S <: Seq[T]](value: Expectable[S]): MatchResult[S] = {
			result(
				{ value.value.forall {seq.contains(_)} },
				{ value.value.toString + " was not subset of expected values" },
				{ value.value.toString + " was subset of expected values" },
				value
			)
		}
	}


	def oneOf[T](acceptedValues: T*): OneOfMatcher[T] = new OneOfMatcher(acceptedValues.toSeq)
	def oneOf[T](i: Iterator[T]): OneOfMatcher[T] = new OneOfMatcher(i.toSeq)

	class OneOfMatcher[T](values: Seq[T]) extends Matcher[Seq[T]] {
		override def apply[S <: Seq[T]](value: Expectable[S]): MatchResult[S] = {
			result(
				{ values.contains(value.value) },
				{ value.value.toString + " was not one of expected values" },
				{ value.value.toString + " was one of expected values" },
				value
			)
		}
	}
}

object CustomMatchers extends CustomMatchers