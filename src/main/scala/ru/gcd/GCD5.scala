package ru.gcd

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 02/12/2012
 */

class GCD5 extends ShouldMatchers {

	@Test def shouldFindGreatestCommonDivider() {
		gcdOf(1, 1) should equal(1)

		gcdOf(2, 1) should equal(1)
		gcdOf(1, 2) should equal(1)

		gcdOf(2, 4) should equal(2)
		gcdOf(4, 2) should equal(2)
	}

	def gcdOf(n1: Int, n2: Int): Int = {
		1
	}
}