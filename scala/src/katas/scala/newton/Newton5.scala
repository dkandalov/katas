package katas.scala.newton

import org.junit.Test
import org.specs2.matcher.{Expectable, MatchResult, Matcher, ShouldMatchers}

/**
 * User: dima
 * Date: 20/04/2013
 */

class Newton5 extends ShouldMatchers {
	@Test def `should find square root of a number using Newton method`() {
		squareRootOf(1) should beCloseTo(1.0)
		squareRootOf(4) should beCloseTo(2.0)
		squareRootOf(10) should beCloseTo(3.16227)
	}

	private def squareRootOf(n: Double, guess: Double = 1.0, threshold: Double = 0.0001): Double = {
		def goodEnough = {guess: Double => (n - guess * guess).abs < threshold}
		def improve = {guess: Double => guess - ((guess * guess - n) / (2 * guess))}
		if (goodEnough(guess)) guess
		else squareRootOf(n, improve(guess))
	}

	private def beCloseTo(expected: Double, threshold: Double = 0.0001) = {
		new Matcher[Double] {
			override def apply[S <: Double](left: Expectable[S]): MatchResult[S] =
				result(
					{(left.value - expected).abs < threshold},
					{ left + " is not close to " + right },
					{ left + " is close to " + right },
					left
				)
		}
	}
}