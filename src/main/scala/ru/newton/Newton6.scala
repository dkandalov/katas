package ru.newton

import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalautils.TolerantNumerics.tolerantDoubleEquality


class Newton6 extends Matchers {
	@Test def `find square root of number with Newton's method`() {
		newtonSqrt(1) should equal(1)
		newtonSqrt(4) should beTolerantEqual(2)
		newtonSqrt(5) should beTolerantEqual(2.2360688956)
	}

	private def newtonSqrt(n: Double, m: Double = 1, tolerance: Double = 0.001): Double = {
		if ((m * m - n).abs < tolerance) m
		else newtonSqrt(n, m - ((m * m - n) / (2 * m)), tolerance)
	}
}