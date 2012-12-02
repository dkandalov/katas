package ru.gcd

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import annotation.tailrec

/**
 * User: dima
 * Date: 02/12/2012
 */

class GCD5 extends ShouldMatchers {

	@Test def shouldFindGreatestCommonDivider() {
		gcdOf(1, 1) should equal(1)
		gcdOf(2, 1) should equal(1)
		gcdOf(4, 2) should equal(2)
		gcdOf(9, 6) should equal(3)
	}

	@Test def orderOfArgumentsShouldNotMatter() {
		gcdOf(1, 2) should equal(1)
		gcdOf(2, 1) should equal(1)
	}

	@tailrec private def gcdOf(a: Int, b: Int): Int = {
		if (b == 0) a
		else if (a < b) gcdOf(b, a)
		else gcdOf(b, a % b)
	}
}