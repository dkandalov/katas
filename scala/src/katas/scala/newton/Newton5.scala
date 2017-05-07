package katas.scala.newton

import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

/**
 * User: dima
 * Date: 20/04/2013
 */

class Newton5 extends Matchers {
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

	private def beCloseTo(right: Double, threshold: Double = 0.00001): Matcher[Double] = {
		(left: Double) => {
			MatchResult(
				(left - right).abs < threshold,
				left + " is not close to " + right,
				left + " is close to " + right
			)
		}
	}
}