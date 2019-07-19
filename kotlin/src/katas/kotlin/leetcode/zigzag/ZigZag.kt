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
    (0 until 3).joinToString("") { n ->
        mapIndexedNotNull { i, c ->
            if (i % 3 == n) c else null
        }.joinToString("")
    }
    return this
}
