package katas.scala.newton

import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

/**
 * User: dima
 * Date: 30/03/2013
 */

class Newton2 extends Matchers {

	@Test def shouldFindSquareRootOfANumber() {
		squareRootOf(1.0) should beCloseTo(1.0)
		squareRootOf(2.0) should beCloseTo(1.41421)
		squareRootOf(3.0) should beCloseTo(1.73205)
		squareRootOf(4.0) should beCloseTo(2.0)
	}

	def squareRootOf(d: Double, guess: Double = 1.0, threshold: Double = 0.00001): Double = {
		def isGoodEnough(guess: Double) = ((guess * guess) - d).abs < threshold
		def improve(guess: Double) = guess - ((guess * guess - d) / (2 * guess))

		if (isGoodEnough(guess)) guess
		else squareRootOf(d, improve(guess), threshold)
	}

	private def beCloseTo(right: Double, threshold: Double = 0.00001): Matcher[Double] = {
		(left: Double) => {
			MatchResult(
				(left - right).abs < threshold,
				left + " is not close to " + right,
				left + " is close to " + right)
		}
	}
}