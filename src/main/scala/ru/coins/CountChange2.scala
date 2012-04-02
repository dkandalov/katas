package ru.coins

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 02/04/2012
 */
class CountChange2 extends ShouldMatchers {

  @Test def shouldFindAmountOfUniqueWaysToChangeSumOfMoney() {
    amountOfCombinations(1) should equal(1) // 1
    amountOfCombinations(5) should equal(2) // 1*5; 5
    amountOfCombinations(6) should equal(2) // 1*5; 1,5
    amountOfCombinations(10) should equal(4) // 1*10; 1*5,5; 5,5; 10
    amountOfCombinations(100) should equal(292)
  }

  def amountOfCombinations(sum: Int, coinType: Int = 4): Int = {
    if (coinType < 0 || sum < 0) 0
    else if (sum == 0) 1
    else amountOfCombinations(sum, coinType - 1) +
      amountOfCombinations(sum - coinValue(coinType), coinType)
  }

  @Test def shouldFindAllUniqueWaysToChangeSumOfMoney() {
    combinations(1) should equal(Seq(Seq(1)))
    combinations(5) should equal(Seq(Seq(1, 1, 1, 1, 1), Seq(5)))
    combinations(10) should equal(Seq(times(1, 10), times(1, 5) ++ Seq(5), Seq(5, 5), Seq(10)))
  }

  def times(value: Int, timesToRepeat: Int) = Range(value, timesToRepeat + 1).map { _ => value}.toList

  def combinations(sum: Int, coinType: Int = 4): Seq[Seq[Int]] = {
    if (coinType < 0 || sum < 0) Seq()
    else if (sum == 0) Seq(Seq())
    else combinations(sum, coinType - 1) ++
      combinations(sum - coinValue(coinType), coinType).map { seq => seq.:+(coinValue(coinType))}
  }

  def coinValue(coinType: Int) = coinType match {
    case 0 => 1
    case 1 => 5
    case 2 => 10
    case 3 => 25
    case 4 => 50
  }
}