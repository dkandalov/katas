package katas.kotlin.leetcode.reverse_integer

import kotlincommon.pow
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.lang.NumberFormatException
import kotlin.math.absoluteValue

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
            var n = this
            0.until(10).forEach {
                list.add(n.rem(10))
                n = n.div(10)
            }
            val nn = list.dropLastWhile { it == 0 }.foldIndexed(0, { i, acc, n ->
                acc + n * 10.pow(i)
            })
            try {
                toString().reversed().toInt()
            } catch (e: NumberFormatException) {
                0
            }
        }
    }
}
