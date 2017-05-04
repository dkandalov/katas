package katas.scala.w1

import org.specs2.matcher.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 22/09/2012
 */

class AndOr extends ShouldMatchers {
	@Test def booleanLogic() {
		and(false, false) should equalTo(false)
		and(false, true) should equalTo(false)
		and(true, false) should equalTo(false)
		and(true, true) should equalTo(true)

		or(false, false) should equalTo(false)
		or(false, true) should equalTo(true)
		or(true, false) should equalTo(true)
		or(true, true) should equalTo(true)
	}

	def and(x: Boolean, y: => Boolean) = if (!x) false else y

	def or(x: Boolean, y: => Boolean) = if (x) true else y
}