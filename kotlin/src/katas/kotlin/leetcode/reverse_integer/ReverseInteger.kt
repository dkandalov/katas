package katas.kotlin.leetcode.reverse_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class ReverseInteger {
    @Test fun `positive numbers`() {
        0.reverse() shouldEqual 0
        12.reverse() shouldEqual 21
        120.reverse() shouldEqual 21
    }

    @Test fun `negative numbers`() {
        -1.reverse() shouldEqual -1
    }
}

private fun Int.reverse(): Int {
    if (this < 0) -(-this).reverse()
    return this.toString().reversed().toInt()
}
