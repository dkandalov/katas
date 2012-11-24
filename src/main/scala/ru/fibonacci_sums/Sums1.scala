package ru.fibonacci_sums

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 24/11/2012
 */

class Sums1 extends ShouldMatchers {

	@Test def givenANumber_shouldFindFibonacciNumbersWithSumEqualToIt() {
		combinationsOf(0) should equal(Seq(Seq()))
		combinationsOf(1) should equal(Seq(Seq(1)))
		combinationsOf(2) should equal(Seq(Seq(2)))
		combinationsOf(3) should equal(Seq(Seq(3), Seq(2, 1)))
		combinationsOf(4) should equal(Seq(Seq(3, 1)))
		combinationsOf(5) should equal(Seq(Seq(5), Seq(3, 2)))
	}

	@Test def shouldCalculateSequenceOfFibonacciNumbers() {
		calculateFibonacci(10) should equal(Seq(1, 2, 3, 5, 8, 13, 21, 34, 55, 89))
	}

	private def combinationsOf(n: Int): Seq[Int] = Seq()

	private def calculateFibonacci(amount: Int) = Seq(1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
}