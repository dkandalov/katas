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
	}

	def gcdOf(n1: Int, n2: Int): Int = {
		0
	}
}