package ru.dojo

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

//object Sums {
//	def main(args: Array[String]) {
//		for(i <- 1 to 20) { println(i + " is " + new Sums().fibonacciZeckendorfRepresentationOf(i).mkString(" or "))}
//	}
//}

class Sums extends ShouldMatchers {
	@Test def shouldFindRepresentationsOfOneToThirteen() {
		def result = (for(i <- 1 to 13) yield fibonacciRepresentationOf(i))
		result(0) should equal(Vector("1"))
		result(1) should equal(Vector("10"))
		result(2) should equal(Vector("100", "11"))
		result(3) should equal(Vector("101"))
		result(4) should equal(Vector("1000", "110"))
		result(5) should equal(Vector("1001", "111"))
		result(6) should equal(Vector("1010"))
		result(7) should equal(Vector("10000", "1100", "1011"))
		result(8) should equal(Vector("10001", "1101"))
		result(9) should equal(Vector("10010", "1110"))
		result(10) should equal(Vector("10100", "10011", "1111"))
		result(11) should equal(Vector("10101"))
		result(12) should equal(Vector("100000", "11000", "10110"))
	}

	@Test def shouldFindZeckendorfRepresentationsOfOneToThriteen() {
		def result = (for(i <- 1 to 13) yield fibonacciZeckendorfRepresentationOf(i))
		result(0) should equal(Vector("1"))
		result(1) should equal(Vector("10"))
		result(2) should equal(Vector("100"))
		result(3) should equal(Vector("101"))
		result(4) should equal(Vector("1000"))
		result(5) should equal(Vector("1001"))
		result(6) should equal(Vector("1010"))
		result(7) should equal(Vector("10000"))
		result(8) should equal(Vector("10001"))
		result(9) should equal(Vector("10010"))
		result(10) should equal(Vector("10100"))
		result(11) should equal(Vector("10101"))
		result(12) should equal(Vector("100000"))
	}

	@Test def miscTests() {
		fibonacci should equal(Seq(1, 2, 3, 5, 8, 13))

		combinationsOfLength(1, 5) should equal(Seq(Seq(5)))

		allFibonacciCombinationsOf(5) should equal(Seq(Seq(5), Seq(2,3)))

		asFibonacciString(Seq(Seq(5), Seq(2,3))) should equal(Seq("1000", "110"))
	}

	lazy val fibonacciSize = 6
	lazy val fibonacci = calculateFibonacci(fibonacciSize)

	def fibonacciZeckendorfRepresentationOf(n: Int): Seq[String] = {
		fibonacciRepresentationOf(n).filterNot{ _.contains("11") }
	}

	def fibonacciRepresentationOf(n: Int): Seq[String] = {
		asFibonacciString(allFibonacciCombinationsOf(n))
	}

	private def allFibonacciCombinationsOf(n: Int): Seq[Seq[Int]] = {
		(for (length <- 1 to fibonacciSize) yield combinationsOfLength(length, n)).flatten
	}

	private def combinationsOfLength(count: Int, n: Int) : Seq[Seq[Int]] = {
		fibonacci.combinations(count).filter(xs => xs.sum == n).toList
	}

	private def asFibonacciString(ints: Seq[Seq[Int]]) : Seq[String] = {
		def oneFibonacciString(ints: Seq[Int]) =
			(for (n <- fibonacci) yield if (ints.contains(n)) '1' else '0').reverse.dropWhile(_ == '0').mkString

		ints map oneFibonacciString
	}

	private def calculateFibonacci(count: Int): Seq[Int] = {
		def f(n: Int, current: Int, previous: Int): List[Int] =
		   if (n > 0) current :: f(n - 1, current + previous, current)
		   else Nil
		f(count, 1, 1)
	}

	private def calculateFibonacciStream: Stream[Int] = {
		def f(current: Int, previous: Int): Stream[Int] =
			current #:: f(current + previous, current)
		f(1, 1)
	}

}