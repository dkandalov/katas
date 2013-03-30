package ru.newton

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 30/03/2013
 */

class Newton2 extends ShouldMatchers {

	@Test def shouldFindSquareRootOfANumber() {
		squareRootOf(1.0) should equal(1.0)
		squareRootOf(2.0) should equal(1.4142156862745099)
		squareRootOf(3.0) should equal(1.7320508100147276)
		squareRootOf(4.0) should equal(2.0000000929222947)
	}

	def squareRootOf(d: Double, guess: Double = 1.0, threshold: Double = 0.0001): Double = {
		def isGoodEnough(guess: Double) = ((guess * guess) - d).abs < threshold
		def improve(guess: Double) = guess - ((guess * guess - d) / (2 * guess))

		if (isGoodEnough(guess)) guess
		else squareRootOf(d, improve(guess), threshold)
	}

}