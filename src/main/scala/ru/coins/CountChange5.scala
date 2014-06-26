package ru.coins

import org.scalatest.Matchers
import org.junit.Test


class CountChange5 extends Matchers {
	@Test def `should find ways a sum of money can be changed as given types of coin`() {
		val coinTypes = Seq(1, 5, 10, 25, 50)
		findChange(0, coinTypes) should equal(Seq(Seq()))
		findChange(1, coinTypes) should equal(Seq(Seq(1)))
		findChange(5, coinTypes) should equal(Seq(
			Seq(1, 1, 1, 1, 1),
			Seq(5)
		))
		findChange(10, coinTypes) should equal(Seq(
			Seq(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
			Seq(1, 1, 1, 1, 1, 5),
			Seq(5, 5),
			Seq(10)
		))
		findChange(100, coinTypes).size should equal(292)
	}

	private def findChange(amount: Int, coinTypes: Seq[Int]): Seq[Seq[Int]] = {
		if (amount == 0) return Seq(Seq())
		if (amount < 0 || coinTypes.isEmpty) return Seq()

		val coinType = coinTypes.head
		findChange(amount - coinType, coinTypes).map{ coinType +: _ } ++
		findChange(amount, coinTypes.tail)
	}
}