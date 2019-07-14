package katas.kotlin.leetcode.reverse_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class ReverseInteger {
    @Test fun `positive numbers`() {
        0.reverse() shouldEqual 0
    }
}

private fun Int.reverse(): Int {
    return this
}
