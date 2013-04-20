package ru.newton

import org.scalatest.matchers.{MatchResult, Matcher, ShouldMatchers}
import org.junit.Test

/**
 * User: dima
 * Date: 04/04/2013
 */

class Newton4 extends ShouldMatchers {
	@Test def `should find square root of a number`() {
		squareRootOf(1.0) should beCloseTo(1.0)
		squareRootOf(2.0) should beCloseTo(1.41421)
		squareRootOf(9.0) should beCloseTo(3.0)
		squareRootOf(10.0) should beCloseTo(3.16227)
	}

	def squareRootOf(n: Double, guess: Double = 1, threshold: Double = 0.0001): Double = {
		def guessIsGoodEnough = (n - guess * guess).abs < threshold
		def improvedGuess = guess - ((guess * guess - n) / (2 * guess))v

		if (guessIsGoodEnough) guess else squareRootOf(n, improvedGuess)
	}

	def beCloseTo(right: Double, threshold: Double = 0.0001) =
		new Matcher[Double] {
			override def apply(left: Double) =
				MatchResult(
					(left - right).abs < threshold,
					left + " is not close to " + right,
					left + " is close to " + right
				)
		}
}