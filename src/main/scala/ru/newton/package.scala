package ru

import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalautils.TolerantNumerics._

package object newton {
	def beTolerantEqualTo(expected: Double, tolerance: Double = 0.001): Matcher[Double] = {
		val equality = tolerantDoubleEquality(tolerance)
		new Matcher[Double] {
			override def apply(actual: Double): MatchResult =
				MatchResult(
					equality.areEqual(expected, actual),
					s"expected $actual to be equal $expected (with tolerance $tolerance)",
					s"$actual is not equal $expected (with tolerance $tolerance)"
				)
		}
	}
}
