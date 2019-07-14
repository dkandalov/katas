package katas.kotlin.leetcode.reverse_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class ReverseInteger {
    @Test fun `positive numbers`() {
        0.reverse() shouldEqual 0
        12.reverse() shouldEqual 21
        120.reverse() shouldEqual 21
    }
}

private fun Int.reverse(): Int {
    return this.toString().reversed().toInt()
}
