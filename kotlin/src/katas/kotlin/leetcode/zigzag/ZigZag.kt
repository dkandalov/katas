package katas.kotlin.leetcode.zigzag

import kotlincommon.test.shouldEqual
import org.junit.Test

class ZigZag {
    @Test fun `zigzag first column`() {
        "ABC".zigzag() shouldEqual "ABC"
    }

    @Test fun `zigzag one cycle`() {
        // A
        // B D
        // C
        "ABCD".zigzag() shouldEqual "ABDC"
    }

    @Test fun `zigzag two cycles`() {
        // A   E
        // B D
        // C
        "ABCDE".zigzag() shouldEqual "AEBDC"
    }
}

private fun String.zigzag(): String {
    return listOf(0, 1, 3, 2).joinToString("") { n ->
        mapIndexedNotNull { i, c ->
            if (i.rem(4) == n) c else null
        }.joinToString("")
    }
}
