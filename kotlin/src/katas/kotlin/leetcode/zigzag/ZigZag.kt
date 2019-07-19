package katas.kotlin.leetcode.zigzag

import kotlincommon.test.shouldEqual
import org.junit.Test

class ZigZag {
    @Test fun `zigzag first column`() {
        "ABC".zigzag() shouldEqual "ABC"
    }

//    @Test fun `zigzag one cycle`() {
//        // A
//        // B D
//        // C
//        "ABCD".zigzag() shouldEqual "ABDC"
//    }
}

private fun String.zigzag(): String {
    listOf(0, 1, 3, 2).joinToString("") { n ->
        mapIndexedNotNull { i, c ->
            if (i % 4 == n) c else null
        }.joinToString("")
    }
    return this
}
