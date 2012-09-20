package ru.dojo

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test


class CountChange extends ShouldMatchers {
	@Test def aaa() {
		val coins = List(2, 1)

		calculateChanges(0, coins) should equal(List())
		countChange(0, coins) should equal(0)

		calculateChanges(1, coins) should equal(List(List(1)))
		countChange(1, coins) should equal(1)

		calculateChanges(2, coins) should equal(List(List(2), List(1, 1)))
		countChange(2, coins) should equal(2)

		countChange(4, coins) should equal(3)
		calculateChanges(4, coins) should equal(List(List(2, 2), List(2, 1, 1), List(1, 1, 1, 1)))
	}

	def countChange(money: Int, coins: List[Int]): Int = {
		calculateChanges(money, coins).size
	}

	def calculateChanges(money: Int, coins: List[Int]): List[List[Int]] = {
		if (coins.isEmpty) return List()
		calc(money, coins) ++ calculateChanges(money, coins.tail)
	}

	def calc(money: Int, coins: List[Int]): List[List[Int]] = {
		if (coins.isEmpty) return List()
		if (money == coins.head) return List(List(coins.head))
		if (money < coins.head) calculateChanges(money, coins.tail)
		else calculateChanges(money - coins.head, coins).map( list => coins.head +: list) ++ calculateChanges(money, coins.tail)
	}
}