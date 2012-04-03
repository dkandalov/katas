package ru.coins

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import runtime.RichInt
import ru.util.Pomodoro

/**
 * User: dima
 * Date: 03/04/2012
 */
@Pomodoro("1")
class CountChange3 extends ShouldMatchers {
  @Test def aaa() {
    // TODO implicit conversion for nicer int ranges
  }

  @Test def GIVEN_amount_of_money_SHOULD_find_all_unique_ways_to_change_it() {
    waysToChange(1).size should equal(1)
    waysToChange(1) should equal(Seq(Seq(1)))

    waysToChange(5).size should equal(2)
    waysToChange(5) should equal(Seq(Seq(1, 1, 1, 1, 1), Seq(5)))

    waysToChange(10) should equal(Seq(Seq(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),Seq(1, 1, 1, 1, 1, 5), Seq(5, 5), Seq(10)))

    waysToChange(100).size should equal(292)
  }

  def waysToChange(money: Int, coinType: Int = 4): Seq[Seq[Int]] = {
    if (money < 0 || coinType < 0) Seq()
    else if (money == 0) Seq(Seq())
    else waysToChange(money, coinType - 1) ++
      waysToChange(money - valueOf(coinType), coinType).map{ seq => seq.:+(valueOf(coinType))}
  }

  def valueOf(coinType: Int): Int = coinType match {
    case 0 => 1
    case 1 => 5
    case 2 => 10
    case 3 => 25
    case 4 => 50
  }

  class Aaa(i: Int) {
    def times(n: Int) = Seq.fill(n) {i}
  }
}