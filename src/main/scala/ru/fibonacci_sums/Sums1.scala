package ru.fibonacci_sums

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 24/11/2012
 */

class Sums1 extends ShouldMatchers {

	@Test def givenANumberShouldFindItFibonacciRepresentaions() {
		fibonacciRepresentationsOf(0) should equal(Seq(""))
		fibonacciRepresentationsOf(1) should equal(Seq("1"))
		fibonacciRepresentationsOf(2) should equal(Seq("10"))
		fibonacciRepresentationsOf(3) should equal(Seq("100", "11"))
	}

	@Test def givenANumber_shouldFindFibonacciNumbersWithSumEqualToIt() {
		fibonacciCombinationsOf(0) should equal(Seq(Seq()))
		fibonacciCombinationsOf(1) should equal(Seq(Seq(1)))
		fibonacciCombinationsOf(2) should equal(Seq(Seq(2)))
		fibonacciCombinationsOf(3) should equal(Seq(Seq(3), Seq(2, 1)))
		fibonacciCombinationsOf(4) should equal(Seq(Seq(3, 1)))
		fibonacciCombinationsOf(5) should equal(Seq(Seq(5), Seq(3, 2)))
	}

	@Test def shouldCalculateSequenceOfFibonacciNumbers() {
		calculateFibonacci(10) should equal(Seq(1, 2, 3, 5, 8, 13, 21, 34, 55, 89))
	}

	def fibonacciRepresentationsOf(n: Int): Seq[String] = {
		fibonacciCombinationsOf(n) map combinationAsString
	}

	def combinationAsString(numbers: Seq[Int]): String = {
		val fibs = calculateFibonacci(10).takeWhile{_ <= numbers.head}.reverse
//		fibs.map{}

		""
	}

	private def fibonacciCombinationsOf(n: Int): Seq[Seq[Int]] = {
		def f(n: Int, fibs: Seq[Int]): Seq[Seq[Int]] = {
			if (n == 0) Seq(Seq())
			else {
				val filteredFibs = fibs.takeWhile{_ <= n}
				if (filteredFibs.isEmpty) Seq()
				else (f(n - filteredFibs.last, filteredFibs.init).map{filteredFibs.last +: _}) ++ (f(n, filteredFibs.init))
			}
		}
		f(n, calculateFibonacci(10))
	}

	private def calculateFibonacci(amount: Int) = Seq(1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
}