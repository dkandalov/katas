package katas.scala.dojo

import org.specs2.matcher.ShouldMatchers
import org.junit.Test
import annotation.tailrec

/**
 * User: dima
 * Date: 20/09/2012
 */

class Paren extends ShouldMatchers {
	@Test def aaa() {
		balance("".toList) should equalTo(true)

		balance("(if (zero? x) max (/ 1 x))".toList) should equalTo(true)
		balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList) should equalTo(true)
		balance(":-)".toList) should equalTo(false)
		balance("())(".toList) should equalTo(false)
	}

	def balance(chars: List[Char]): Boolean = {
		recursive_balance(0, chars)
	}

	@tailrec private def recursive_balance(count: Int, remaining: List[Char]): Boolean = {
		if (count < 0) false
		else if (remaining.isEmpty) true
		else recursive_balance(count + charWeight(remaining.head), remaining.tail)
	}

	private def charWeight(char: Char): Int = char match {
		case '(' => 1
		case ')' => -1
		case _ => 0
	}
}