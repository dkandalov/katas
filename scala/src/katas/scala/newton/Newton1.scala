package katas.scala.newton

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

/**
 * User: dima
 * Date: 29/03/2013
 */

class Newton1 extends ShouldMatchers {
	@Test def aa() {
		squareRootOf(1) should equalTo(1.0)
		squareRootOf(2) should equalTo(1.4142156862745099)
		squareRootOf(10) should equalTo(3.162277665175675)
	}

	def squareRootOf(n: Double, guess: Double = 1.0, threshold: Double = 0.00001): Double = {
		def improve(guess: Double) = guess - ((guess * guess - n) / (2 * guess))

		if ((guess * guess - n).abs < threshold) guess
		else squareRootOf(n, improve(guess))
	}

}