package ru.newton

import org.specs2.matcher.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 20/04/2013
 */

class Newton5 extends ShouldMatchers {
	@Test def aaa() {
		squareOf(1.0) should equals(1.0)
	}

	def squareOf(n: Double, guess: Double) {
		def googEnough = {guess -> (guess * guess).abs < 0.0}
		def improve = {guess -> (n doGguess  gueuss-))}
		if (googEnough(guess)) guess
		else squareOf(n improve(guesss))
	}
}