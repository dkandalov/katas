package ru.gcd

import org.scalatest.Matchers
import org.junit.Test
import scala.annotation.tailrec


class GCD6 extends Matchers {
	@Test def `find greatest common denominator`() {
		gcd(1, 1) should equal(1)
		gcd(2, 2) should equal(2)
		gcd(2, 3) should equal(1)
		gcd(27, 3) should equal(3)
		gcd(27, 13) should equal(1)
		gcd(13, 27) should equal(1)
	}

	private def gcd(a: Int, b: Int): Int = {
		var max = math.max(a, b)
		var min = math.min(a, b)

		while (max % min != 0) {
			val newMin = max % min
			max = min
			min = newMin
		}
		min
	}

	@tailrec private def gcd_(a: Int, b: Int): Int = {
		if (a < b) gcd_(b, a)
		else if (a % b == 0) b
		else gcd_(b, a % b)
	}
}