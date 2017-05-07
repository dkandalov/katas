package katas.scala.newton

import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

/**
 * User: dima
 * Date: 04/04/2013
 */

class Newton4 extends Matchers {
	@Test def `should find square root of a number`() {
		squareRootOf(1 + 1) should beCloseTo(0)
		squareRootOf(1.0) should beCloseTo(1.0)
		squareRootOf(2.0) should beCloseTo(1.41421)
		squareRootOf(9.0) should beCloseTo(3.0)
		squareRootOf(10.0) should beCloseTo(3.16227)
	}

	def squareRootOf(n: Double, guess: Double = 1, threshold: Double = 0.0001): Double = {
		def guessIsGoodEnough = (n - guess * guess).abs < threshold
		def improvedGuess = guess - ((guess * guess - n) / (2 * guess))

		if (guessIsGoodEnough) guess else squareRootOf(n, improvedGuess)
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