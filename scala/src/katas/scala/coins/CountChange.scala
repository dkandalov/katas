package katas.scala.coins

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/**
 * User: DKandalov
 */

class CountChange extends AssertionsForJUnit { // TODO elaborate
  @Test def aaa() {
    println(countChange(100))
  }

  def countChange(amount: Int, kindOfCoins: Int = 5): Int = {
    if (amount == 0) return 1
    if (amount < 0 || kindOfCoins == 0) return 0
    countChange(amount, kindOfCoins - 1) + countChange(amount - denomination(kindOfCoins), kindOfCoins)
  }

  def denomination(coinKind: Int): Int = {
    coinKind match {
      case 1 => 1
      case 2 => 5
      case 3 => 10
      case 4 => 25
      case 5 => 50
    }
  }
}