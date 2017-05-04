package katas.scala.coins

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class CountChange4 extends ShouldMatchers {
	@Test def shouldFindWayToChangeSumOfMoneyGivenASetOfCoins() {
		val withCoinTypes = Seq(1, 5, 10, 25, 50)
		waysToChange(0, withCoinTypes) should equalTo(Seq(Seq()))
		waysToChange(1, withCoinTypes) should equalTo(Seq(Seq(1)))
		waysToChange(5, withCoinTypes) should equalTo(Seq(
			Seq(1, 1, 1, 1, 1),
			Seq(5)
		))
		waysToChange(10, withCoinTypes) should equalTo(Seq(
			Seq(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
			Seq(1, 1, 1, 1, 1, 5),
			Seq(5, 5),
			Seq(10)
		))
		waysToChange(100, withCoinTypes).size should equalTo(292)
	}

	def waysToChange(amount: Int, coinTypes: Seq[Int], index: Int = 0): Seq[Seq[Int]] = {
		if (amount == 0) Seq(Seq())
		else if (amount < 0 || index >= coinTypes.size) Seq()
		else
			waysToChange(amount - coinTypes(index), coinTypes, index).map{ coinTypes(index) +: _ } ++
			waysToChange(amount, coinTypes, index + 1)
	}
}