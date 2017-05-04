package katas.scala.fibonacci

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.annotation.tailrec

/**
 * User: dima
 * Date: 28/07/2012
 */

class Fibonacci6 extends ShouldMatchers {
	@Test def shouldCalculateFibonacciNumbers() {
		fibonacci(0) should equalTo(0)
		fibonacci(1) should equalTo(1)
		fibonacci(2) should equalTo(1)
		fibonacci(3) should equalTo(2)
		fibonacci(4) should equalTo(3)
		fibonacci(5) should equalTo(5)
		fibonacci(6) should equalTo(8)
		fibonacci(7) should equalTo(13)
	}

	@Test def shouldCalculateFibonacci_ForLargeNumbers() {
		fibonacci(20) should equalTo(6765)
		fibonacci(100) should equalTo(BigDecimal("354224848179261915075"))
		fibonacci(200) should equalTo(BigDecimal("2.805711729925101400376119324130389E+41"))
		fibonacci(1000) should equalTo(BigDecimal("4.346655768693745643568852767504065E+208"))
		fibonacci(10000) should equalTo(BigDecimal("3.364476487643178326662161200510745E+2089"))
	}

	@tailrec
	private def fibonacci(n: Int, state: State = State()): BigDecimal = {
		if (state.i >= n) state.current
		else fibonacci(n, state.next())
	}

	@Test def shouldProgressFromOneStateToAnother() {
		def doNext(state: State, times: Int): State = {
			if (times <= 0) state
			else doNext(state.next(), times - 1)
		}

		doNext(State(), 1) should equalTo(State(0, 0, 0))
		doNext(State(), 2) should equalTo(State(1, 0, 1))
		doNext(State(), 3) should equalTo(State(2, 1, 1))
		doNext(State(), 4) should equalTo(State(3, 1, 2))
		doNext(State(), 5) should equalTo(State(4, 2, 3))
		doNext(State(), 6) should equalTo(State(5, 3, 5))
	}

	case class State(i: Int = -1, last: BigDecimal = 0, current: BigDecimal = 0) {
		def next() = {
			if (i == -1) State(0, 0, 0)
			else if (i == 0) State(1, 0, 1)
			else State(i + 1, current, last + current)
		}
	}

	def fibonacci_iterative(n: Int): BigDecimal = {
		if (n == 0) return 0
		if (n == 1) return 1

		var prev = BigDecimal(0)
		var result = BigDecimal(1)
		for (i <- 2 to n) {
			val tmp = result
			result = result + prev
			prev = tmp
		}
		result
	}

	def fibonacci_recursive_exponential(n: Int): Int = n match {
		case 0 => 0
		case 1 => 1
		case _ => fibonacci_recursive_exponential(n - 1) + fibonacci_recursive_exponential(n - 2)
	}
}