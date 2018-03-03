package katas.scala.roman

import java.io.PrintWriter

import katas.scala.roman.Roman.asRoman
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 * User: DKandalov
 */

class Roman extends AssertionsForJUnit {
  @Test def shouldConvertArabNumbersToRoman() {
    assert(Range(1, 10).toList.map(asRoman) === List("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"))
    assert(List(10, 11, 12).map(asRoman) === List("X", "XI", "XII"))
    assert(asRoman(49) === "XLIX")
    assert(asRoman(51) === "LI")
    assert(asRoman(81) === "LXXXI")
    assert(asRoman(91) === "XCI")
    assert(asRoman(99) === "XCIX")
    assert(asRoman(118) === "CXVIII")
    assert(asRoman(399) === "CCCXCIX")
    assert(asRoman(488) === "CDLXXXVIII")
    assert(asRoman(588) === "DLXXXVIII")
    assert(asRoman(999) === "CMXCIX")
    assert(asRoman(1234) === "MCCXXXIV")
  }
}

object Roman {
  def asRoman(n: Int): String = {
    if (n < 4)
      "I" * n
    else if (n == 4)
      "IV"
    else if (n >= 5 && n < 9)
      "V" + asRoman(n - 5)
    else if (n == 9)
      "IX"
    else if (n >= 10 && n < 40)
      "X" + asRoman(n - 10)
    else if (n >= 40 && n < 50)
      "XL" + asRoman(n - 40)
    else if (n >= 50 && n < 90)
      "L" + asRoman(n - 50)
    else if (n >= 90 && n < 100)
      "XC" + asRoman(n - 90)
    else if (n >= 100 && n < 400)
      "C" + asRoman(n - 100)
    else if (n >= 400 && n < 500)
      "CD" + asRoman(n - 400)
    else if (n >= 500 && n < 900)
      "D" + asRoman(n - 500)
    else if (n >= 900 && n < 1000)
      "CM" + asRoman (n - 900)
    else if (n >= 1000)
      "M" + asRoman(n - 1000)
    else
      throw new IllegalArgumentException()
  }

  def main(args: Array[String]) {
    val writer = new PrintWriter("roman_numbers.txt")
    Range(1, 3000).foreach{ n =>
      writer.println(asRoman(n))
      0
    }
  }
}