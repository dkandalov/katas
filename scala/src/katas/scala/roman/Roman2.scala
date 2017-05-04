package katas.scala.roman

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

/**
 * User: dima
 * Date: 23/05/2012
 */

class Roman2 extends ShouldMatchers {
	@Test def shouldConvertNumbersToRomanNumerals() {
		toRoman(1) should equalTo("I")
		toRoman(2) should equalTo("II")
		toRoman(4) should equalTo("IV")
		toRoman(5) should equalTo("V")
		toRoman(9) should equalTo("IX")
		toRoman(10) should equalTo("X")
		toRoman(40) should equalTo("XL")
		toRoman(41) should equalTo("XLI")
		toRoman(49) should equalTo("XLIX")
		toRoman(50) should equalTo("L")

		toRoman(99) should equalTo("XCIX")
		toRoman(118) should equalTo("CXVIII")
		toRoman(399) should equalTo("CCCXCIX")
		toRoman(488) should equalTo("CDLXXXVIII")
		toRoman(588) should equalTo("DLXXXVIII")
		toRoman(999) should equalTo("CMXCIX")
		toRoman(1234) should equalTo("MCCXXXIV")
	}

	val numerals = Seq(
		(1000, "M", 100),
		(500, "D", 100),
		(100, "C", 10),
		(50, "L", 10),
		(10, "X", 1),
		(5, "V", 1),
		(1, "I", Int.MinValue)
	)
	
	def toRoman(n: Int): String = {
		def convert(n: Int, nums: Seq[(Int,String,Int)]): String = {
			if (n == 0) return ""
			val numeral = nums.head

			if (numeral._1 - n > 0 && numeral._1 - n <= numeral._3) {
				convert(numeral._3, nums.tail) + numeral._2 + convert(n % numeral._3, nums.tail)
			} else if (n / numeral._1 > 0) {
				numeral._2 * (n / numeral._1) + convert(n % numeral._1, nums)
			} else {
				convert(n, nums.tail)
			}
		}
		
		if (n <= 0) throw new IllegalArgumentException("n must be > 0 but was " + n)
		convert(n, numerals)
	}
}