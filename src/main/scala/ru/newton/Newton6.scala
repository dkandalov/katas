package ru.newton

import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalautils.TolerantNumerics.tolerantDoubleEquality


class Newton6 extends Matchers {
	@Test def `find square root of number with Newton's method`() {
		newtonSqrt(1) should equal(1)
		newtonSqrt(4) should tolerantEqual(2)
		newtonSqrt(5) should tolerantEqual(2.2360688956)
	}

	private def newtonSqrt(n: Double, m: Double = 1, tolerance: Double = 0.001): Double = {
		if ((m * m - n).abs < tolerance) m
		else newtonSqrt(n, m - ((m * m - n) / (2 * m)), tolerance)
	}

	private def tolerantEqual(expected: Double, tolerance: Double = 0.001): Matcher[Double] = {
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