package katas.scala.fibonacci_sums

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

/**
 * User: dima
 * Date: 24/11/2012
 */

class Sums2 extends ShouldMatchers {

	@Test def output() {
		for (i <- 1 to 20) {
			println(fibonacciRepresentationsOf(i))
		}
	}

	@Test def givenANumberShouldFindItFibonacciRepresentaions() {
		fibonacciRepresentationsOf(0) should equalTo(Seq(""))
		fibonacciRepresentationsOf(1) should equalTo(Seq("1"))
		fibonacciRepresentationsOf(2) should equalTo(Seq("10"))
		fibonacciRepresentationsOf(3) should equalTo(Seq("100", "11"))
	}

	@Test def givenANumber_shouldFindFibonacciNumbersWithSumEqualToIt() {
		fibonacciCombinationsOf(0) should equalTo(Seq(Seq()))
		fibonacciCombinationsOf(1) should equalTo(Seq(Seq(1)))
		fibonacciCombinationsOf(2) should equalTo(Seq(Seq(2)))
		fibonacciCombinationsOf(3) should equalTo(Seq(Seq(3), Seq(2, 1)))
		fibonacciCombinationsOf(4) should equalTo(Seq(Seq(3, 1)))
		fibonacciCombinationsOf(5) should equalTo(Seq(Seq(5), Seq(3, 2)))
	}

	@Test def shouldCalculateSequenceOfFibonacciNumbers() {
		fibonacciSequenceFor(10) should equalTo(Seq(1, 2, 3, 5, 8))
		fibonacciSequenceFor(100) should equalTo(Seq(1, 2, 3, 5, 8, 13, 21, 34, 55, 89))
	}

	def fibonacciRepresentationsOf(n: Int): Seq[String] = {
		fibonacciCombinationsOf(n) map combinationAsString
	}

	private def combinationAsString(combination: Seq[Int]): String = {
		if (combination.isEmpty) ""
		else {
			val fibs = fibonacciSequenceFor(combination.head).reverse
			fibs.map{ fibNumber => if (combination.contains(fibNumber)) "1" else "0"}.mkString
		}
	}

	private def fibonacciCombinationsOf(n: Int): Seq[Seq[Int]] = {
		def f(n: Int, fibs: Seq[Int]): Seq[Seq[Int]] = {
			if (n == 0) Seq(Seq())
			else {
				if (fibs.isEmpty) Seq()
				else (f(n - fibs.last, fibs.init).map{ fibs.last +: _}) ++ (f(n, fibs.init))
			}
		}
		f(n, fibonacciSequenceFor(n))
	}

	var fibonacci = Seq[Int]()

	private def fibonacciSequenceFor(limit: Int): Seq[Int] = {
		if (fibonacci.isEmpty || fibonacci.last < limit) fibonacci = calculateFibonacci(limit)
		fibonacci.takeWhile{_ <= limit}
	}

	private def calculateFibonacci(limit: Int, current: Int = 1, previous: Int = 1): Seq[Int] = {
		if (limit < current) Seq()
		else current +: calculateFibonacci(limit, current + previous, current)
	}
}