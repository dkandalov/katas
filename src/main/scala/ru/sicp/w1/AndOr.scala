package ru.sicp.w1

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 22/09/2012
 */

class AndOr extends ShouldMatchers {
	@Test def booleanLogic() {
		and(false, false) should equal(false)
		and(false, true) should equal(false)
		and(true, false) should equal(false)
		and(true, true) should equal(true)

		or(false, false) should equal(false)
		or(false, true) should equal(true)
		or(true, false) should equal(true)
		or(true, true) should equal(true)
	}

	def and(x: Boolean, y: => Boolean) = if (!x) false else y

	def or(x: Boolean, y: => Boolean) = if (x) true else y
}