package katas.scala.gcd

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.annotation.tailrec


class GCD6 extends ShouldMatchers {
	@Test def `find greatest common denominator`() {
		gcd(1, 1) should equalTo(1)
		gcd(2, 2) should equalTo(2)
		gcd(2, 3) should equalTo(1)
		gcd(27, 3) should equalTo(3)
		gcd(27, 13) should equalTo(1)
		gcd(13, 27) should equalTo(1)
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