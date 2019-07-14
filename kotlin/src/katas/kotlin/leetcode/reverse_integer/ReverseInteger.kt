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
            var list = mutableListOf<Int>()
            var result = 0
            var n = this
            0.until(10).forEach {
                list.add(n.rem(10))
                result += n.rem(10) * 10.pow(it)
                n = n.div(10)
            }
            result.printed("result=")
            val nn = list.dropLastWhile { it == 0 }.asReversed().foldIndexed(0, { i, acc, n ->
                val result = acc + n * 10.pow(i)
                if (result.sign != 0 && acc.sign != 0 && result.sign != acc.sign) return 0
                result
            })
            nn
        }
    }
}
