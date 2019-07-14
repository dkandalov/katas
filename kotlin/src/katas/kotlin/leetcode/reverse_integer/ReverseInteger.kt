package katas.kotlin.leetcode.reverse_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

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
    var result = 0L
    var n = this
    while (n != 0) {
        result = result * 10 + n.rem(10)
        if (result < Int.MIN_VALUE || result > Int.MAX_VALUE) return 0
        n /= 10
    }
    return result.toInt()
}
