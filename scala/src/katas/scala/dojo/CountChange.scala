package katas.scala.dojo

import org.specs2.matcher.ShouldMatchers
import org.junit.Test


class CountChange extends ShouldMatchers {
	@Test def aaa() {
		val coins = List(2, 1)

		calculateChanges(0, coins) should equalTo(List())
		countChange(0, coins) should equalTo(0)

		calculateChanges(1, coins) should equalTo(List(List(1)))
		countChange(1, coins) should equalTo(1)

		calculateChanges(2, coins) should equalTo(List(List(2), List(1, 1)))
		countChange(2, coins) should equalTo(2)

		countChange(4, coins) should equalTo(3)
		calculateChanges(4, coins) should equalTo(List(List(2, 2), List(1, 1, 2), List(1, 1, 1, 1)))

		countChange(100, List(50, 25, 10, 5, 1)) should equalTo(292)
	}

	def countChange(money: Int, coins: List[Int]): Int = {
		calculateChanges(money, coins).size
	}

	def calculateChanges(money: Int, coins: List[Int]): List[List[Int]] = {
		if (money == 0 || coins.isEmpty) List()
		else calc(money, coins)
	}

	def calc(money: Int, coins: List[Int]): List[List[Int]] = {
		if (money == 0) List(List())
		else if (money < 0) List()
		else if (coins.isEmpty) List()
		else calc(money - coins.head, coins).map(_ :+ coins.head) ++ calc(money, coins.tail)
	}
}