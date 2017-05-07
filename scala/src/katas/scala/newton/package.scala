package katas.scala

import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalactic.TolerantNumerics.tolerantDoubleEquality

package object newton {
	def beTolerantEqualTo(expected: Double, tolerance: Double = 0.001): Matcher[Double] = {
		val equality = tolerantDoubleEquality(tolerance)
		(actual: Double) => MatchResult(
			equality.areEqual(expected, actual),
			s"expected $actual to be equal $expected (with tolerance $tolerance)",
			s"$actual is not equal $expected (with tolerance $tolerance)"
		)
	}
}
