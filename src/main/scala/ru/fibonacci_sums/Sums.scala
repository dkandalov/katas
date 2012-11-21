package ru.fibonacci_sums

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import annotation.tailrec

/**
 * User: dima
 * Date: 22/11/2012
 */

class Sums extends ShouldMatchers {
	@Test def shouldFindFibonacciRepresentationsOfANumber() {
		fibonacciRepresentationsOf(1) map asString should equal(Seq("1"))
		fibonacciRepresentationsOf(2) map asString should equal(Seq("10"))
		fibonacciRepresentationsOf(3) map asString should equal(Seq("100", "11"))
		fibonacciRepresentationsOf(4) map asString should equal(Seq("101"))
		fibonacciRepresentationsOf(5) map asString should equal(Seq("1000", "110"))
		fibonacciRepresentationsOf(6) map asString should equal(Seq("1001", "111"))
		fibonacciRepresentationsOf(7) map asString should equal(Seq("1010"))
		fibonacciRepresentationsOf(8) map asString should equal(Seq("10000", "1100", "1011"))
		fibonacciRepresentationsOf(9) map asString should equal(Seq("10001", "1101"))
		fibonacciRepresentationsOf(10) map asString should equal(Seq("10010", "1110"))
		fibonacciRepresentationsOf(11) map asString should equal(Seq("10100", "10011", "1111"))
		fibonacciRepresentationsOf(12) map asString should equal(Seq("10101"))
		fibonacciRepresentationsOf(13) map asString should equal(Seq("100000", "11000", "10110"))
	}

	@Test def shouldCalculateFibonacciNumbers() {
		fibonacciNumbers.take(10) should equal(Seq(1, 2, 3, 5, 8, 13, 21, 34, 55, 89))
	}

	lazy val fibonacciNumbers = calculateFibonacciNumbers(40)

	def asString(fibonacciRepresentation: Seq[Int]) = {
		(for (i <- 0 to fibonacciRepresentation.head)
			yield if (fibonacciRepresentation.contains(i)) "1" else "0").reverse.mkString
	}

	def fibonacciRepresentationsOf(n: Int, fibs: Seq[Int] = fibonacciNumbers): Seq[Seq[Int]] = {
		if (n == 0) Seq(Seq())
		else {
			val nums = fibs.takeWhile(_ <= n)
			if (nums.isEmpty) Seq()
			else (fibonacciRepresentationsOf(n - nums.last, nums.init).map{(nums.size - 1) +: _}) ++
					 (fibonacciRepresentationsOf(n, nums.init))
		}
	}

	def calculateFibonacciNumbers(n: Int): Seq[Int] = {
		@tailrec def f(count: Int, current: Int, previous: Int, result: Seq[Int]): Seq[Int] = {
			if (count == 0) result
			else f(count - 1, current + previous, current, current +: result)
		}
		f(n, 1, 1, Seq()).reverse
	}
}