package katas.kotlin.leetcode.reverse_integer

import kotlincommon.pow
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.lang.NumberFormatException
import kotlin.math.absoluteValue
import kotlin.math.sign

class ReverseInteger {
    @Test fun `positive numbers`() {
        0.reverse() shouldEqual 0
        1.reverse() shouldEqual 1
        12.reverse() shouldEqual 21
        123.reverse() shouldEqual 321

        102.reverse() shouldEqual 201
        120.reverse() shouldEqual 21
        1200.reverse() shouldEqual 21
    }

    @Test fun `negative numbers`() {
        (-1).reverse() shouldEqual -1
        (-12).reverse() shouldEqual -21
        (-123).reverse() shouldEqual -321

        (-102).reverse() shouldEqual -201
        (-120).reverse() shouldEqual -21
        (-1200).reverse() shouldEqual -21
    }

    @Test fun `integer overflow`() {
        Int.MAX_VALUE.reverse() shouldEqual 0
        (Int.MAX_VALUE - 1).reverse() shouldEqual 0
        Int.MIN_VALUE.reverse() shouldEqual 0
        (Int.MIN_VALUE + 1).reverse() shouldEqual 0
    }
}

private fun Int.reverse(): Int {
    return when {
        this == Int.MIN_VALUE -> 0
        this < 0              -> -(absoluteValue.reverse())
        else                  -> {
            var result = 0L
            var mult = 1_000_000_000L
            var n = this
            0.until(10).forEach {
                result += n.rem(10) * mult
                mult /= 10
                n = n.div(10)
            }
            while (result != 0L && result.rem(10) == 0L) result /= 10
            return if (result < Int.MIN_VALUE || result > Int.MAX_VALUE) 0 else result.toInt()
        }
    }
}
