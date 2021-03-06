package katas.scala.newton

import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

/**
 * User: dima
 * Date: 31/03/2013
 */

class Newton3 extends Matchers {
	@Test def shouldFindSquareRootOfANumber() {
		squareRootOf(1.0) should beCloseTo(1.0)
		squareRootOf(2.0) should beCloseTo(1.41421)
		squareRootOf(4.0) should beCloseTo(2.0)
	}

	def squareRootOf(n: Double, guess: Double = 1.0, threshold: Double = 0.00001): Double = {
		def goodEnough(guess: Double) = (guess * guess - n).abs < threshold
		def improve(guess: Double) = guess - ((guess * guess - n) / (2 * guess))

		if (goodEnough(guess)) guess
		else squareRootOf(n, improve(guess), threshold)
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