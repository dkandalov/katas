package katas.kotlin.leetcode.zigzag

import kotlincommon.test.shouldEqual
import org.junit.Test

class ZigZagThreeRowTests {
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
        // B D F H
        // C   G
        "ABCDEFGH".zigzag() shouldEqual "AEBDFHCG"
    }
}

class ZigZagFourRowTests {
    @Test fun `zigzag first column`() {
        "ABCD".zigzag4() shouldEqual "ABCD"
    }

    @Test fun `zigzag one cycle`() {
        // A     G
        // B   F
        // C E
        // D
        "ABCDEFG".zigzag4() shouldEqual "AGBFCED"
    }
}

private fun String.zigzag(): String {
    return listOf(listOf(0), listOf(1, 3), listOf(2)).joinToString("") { indices ->
        mapIndexedNotNull { i, c ->
            if (indices.any { i % 4 == it }) c else null
        }.joinToString("")
    }
}

private fun String.zigzag4(): String {
    return listOf(listOf(0), listOf(1, 5), listOf(2, 4), listOf(3)).joinToString("") { indices ->
        mapIndexedNotNull { i, c ->
            if (indices.any { i % 6 == it }) c else null
        }.joinToString("")
    }
}
