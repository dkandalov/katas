package katas.kotlin.leetcode.zigzag

import kotlincommon.test.shouldEqual
import org.junit.Test

class ZigZag {
    @Test fun `zigzag first column`() {
        "ABC".zigzag() shouldEqual "ABC"
    }
}

private fun String.zigzag(): String {
    return this
}
