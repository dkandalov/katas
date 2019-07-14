package katas.kotlin.leetcode.reverse_integer

import kotlincommon.test.shouldEqual
import org.junit.Test
import java.lang.NumberFormatException
import kotlin.math.absoluteValue

class ReverseInteger {
    @Test fun `positive numbers`() {
        0.reverse() shouldEqual 0
        12.reverse() shouldEqual 21
        120.reverse() shouldEqual 21
    }

    @Test fun `negative numbers`() {
        (-1).reverse() shouldEqual -1
        (-123).reverse() shouldEqual -321
    }

    @Test fun `integer overflow`() {
        Int.MAX_VALUE.reverse() shouldEqual 0
    }
}

private fun Int.reverse(): Int {
    return if (this < 0) -(absoluteValue.reverse())
    else try {
        toString().reversed().toInt()
    } catch (e: NumberFormatException) {
        0
    }
}
