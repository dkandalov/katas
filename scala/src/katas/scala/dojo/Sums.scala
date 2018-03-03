package katas.scala.dojo

import org.scalatest.Matchers
import org.junit.Test

//object Sums {
//	def main(args: Array[String]) {
//		for(i <- 1 to 20) { println(i + " is " + new Sums().zeckendorfRepresentationOf(i).mkString(" or "))}
//	}
//}

class Sums extends Matchers {
	@Test def givenANumber_shouldFindItFibonacciRepresentations() {
		fibonacciRepresentationsOf(1) should equal(Seq("1"))
		fibonacciRepresentationsOf(2) should equal(Seq("10"))
		fibonacciRepresentationsOf(3) should equal(Seq("100", "11"))
		fibonacciRepresentationsOf(4) should equal(Seq("101"))
		fibonacciRepresentationsOf(5) should equal(Seq("1000", "110"))
		fibonacciRepresentationsOf(6) should equal(Seq("1001", "111"))
		fibonacciRepresentationsOf(7) should equal(Seq("1010"))
		fibonacciRepresentationsOf(8) should equal(Seq("10000", "1100", "1011"))
		fibonacciRepresentationsOf(9) should equal(Seq("10001", "1101"))
		fibonacciRepresentationsOf(10) should equal(Seq("10010", "1110"))
		fibonacciRepresentationsOf(11) should equal(Seq("10100", "10011", "1111"))
		fibonacciRepresentationsOf(12) should equal(Seq("10101"))
		fibonacciRepresentationsOf(13) should equal(Seq("100000", "11000", "10110"))
	}

	@Test def givenANumber_shouldFindItZeckendorfRepresentation() {
		zeckendorfRepresentationOf(1) should equal("1")
		zeckendorfRepresentationOf(2) should equal("10")
		zeckendorfRepresentationOf(3) should equal("100")
		zeckendorfRepresentationOf(4) should equal("101")
		zeckendorfRepresentationOf(5) should equal("1000")
		zeckendorfRepresentationOf(6) should equal("1001")
		zeckendorfRepresentationOf(7) should equal("1010")
		zeckendorfRepresentationOf(8) should equal("10000")
		zeckendorfRepresentationOf(9) should equal("10001")
		zeckendorfRepresentationOf(10) should equal("10010")
		zeckendorfRepresentationOf(11) should equal("10100")
		zeckendorfRepresentationOf(12) should equal("10101")
		zeckendorfRepresentationOf(13) should equal("100000")
	}

	@Test def givenANumber_shouldFindFibonacciNumbersWithSumEqualToIt() {
		findFibonacciRepresentationsOf(0) should equal(Seq())
		findFibonacciRepresentationsOf(1) should equal(Seq(Seq(1)))
		findFibonacciRepresentationsOf(5) should equal(Seq(Seq(5), Seq(2,3)))
	}

	@Test def givenFibonacciRepresentationOfANumeber_shouldConvertItToString() {
		asString(Seq()) should equal("")
		asString(Seq(1)) should equal("1")
		asString(Seq(5)) should equal("1000")
		asString(Seq(2, 3)) should equal("110")
	}

	@Test def shouldCalculateSequenceOfFibonacciNumbers_SkippingFirstZero() {
		calculateFibonacciSequence(0) should equal(Seq())
		calculateFibonacciSequence(1) should equal(Seq(1))
		calculateFibonacciSequence(2) should equal(Seq(1, 2))
		calculateFibonacciSequence(6) should equal(Seq(1, 2, 3, 5, 8, 13))
	}

	lazy val fibonacciSize = 6
	lazy val fibonacci = calculateFibonacciSequence(fibonacciSize)

	type NumberAsFibs = Seq[Int]

	def zeckendorfRepresentationOf(n: Int): String = {
		fibonacciRepresentationsOf(n).filterNot{ _.contains("11") }.head
	}

	def fibonacciRepresentationsOf(n: Int): Seq[String] = {
		findFibonacciRepresentationsOf(n) map asString
	}

	private def findFibonacciRepresentationsOf(n: Int): Seq[NumberAsFibs] = {
		for (
			length <- 1 to fibonacciSize;
			combination <- fibonacci.combinations(length) if combination.sum == n
		) yield combination
	}

	private def asString(numberAsFibs: NumberAsFibs) = {
		val chars = for (i <- fibonacci) yield if (numberAsFibs.contains(i)) '1' else '0'
		chars.reverse.dropWhile(_ == '0').mkString
	}

	private def calculateFibonacciSequence(count: Int): Seq[Int] = {
		def f(count: Int, current: Int, previous: Int): List[Int] = count match {
			case 0 => List()
			case _ =>
				val newCurrent = current + previous
				val newLast = current
				current :: f(count - 1, newCurrent, newLast)
		}
		f(count, 1, 1)
	}

	private def calculateFibonacciStream: Stream[Int] = {
		def f(current: Int, previous: Int): Stream[Int] =
			current #:: f(current + previous, current)
		f(1, 1)
	}

}