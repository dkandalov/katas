package katas.scala

import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.specs2.matcher.{Expectable, MatchResult, Matcher}

package object newton {
	def beTolerantEqualTo(expected: Double, tolerance: Double = 0.001): Matcher[Double] = {
		val equality = tolerantDoubleEquality(tolerance)
		new Matcher[Double] {
			override def apply[S <: Double](actual: Expectable[S]): MatchResult[S] =
				result(
					{ equality.areEqual(expected, actual) },
					{ s"expected $actual to be equal $expected (with tolerance $tolerance)" },
					{ s"$actual is not equal $expected (with tolerance $tolerance)" },
					actual
				)
		}
	}
}
