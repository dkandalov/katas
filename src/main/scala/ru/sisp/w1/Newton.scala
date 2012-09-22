package ru.sisp.w1

/**
 * User: dima
 * Date: 22/09/2012
 */

object Newton {
	def main(args: Array[String]) {
		println("result " + sqrt(2))
		println("result " + sqrt(4))
		println("result " + sqrt(0.001))   // returns 0.04
		println("result " + sqrt(0.1e-20)) // returns 0.03
//		println(0.03125 * 0.03125 - 0.1e-20)
		println("result " + sqrt(1.0e20)) // correct
		println("result " + sqrt(1.0e50)) // loops with 1.0E25
	}

	def sqrt(x: Double, guess: Double = 1): Double = {
		println(guess)
		if (isGoodEnough(x, guess)) guess
		else sqrt(x, improve(x, guess))
	}

	def isGoodEnough(x: Double, guess: Double) = {
		(guess * guess - x).abs / x < 0.001
	}

	def improve(x: Double, guess: Double) = {
		(guess + x / guess) / 2
	}
}