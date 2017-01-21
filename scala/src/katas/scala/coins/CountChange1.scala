package katas.scala.coins

import org.scalatest.Matchers
import org.junit.Test

/**
 * @author DKandalov
 */
class CountChange1 extends Matchers {

  @Test def shouldCountNumberOfWaysToChangeAmountOfMoney() {
    countChangeVariants(100) should equal(292)
  }

  def countChangeVariants(amount: Int, coinType: Int = 4): Int = {
    if (amount == 0) return 1 // didn't have this statement
    if (amount < 0 || coinType < 0) return 0 // amount was == 0
    countChangeVariants(amount - denominationOf(coinType), coinType) + countChangeVariants(amount, coinType - 1) // decreased amount in the seconds addend too
  }

  def denominationOf(coinType: Int) = coinType match {
    case 0 => 1
    //case 1 => 2 had this... which is real but screws up numbers
    case 1 => 5
    case 2 => 10
    case 3 => 25
    case 4 => 50
  }
}