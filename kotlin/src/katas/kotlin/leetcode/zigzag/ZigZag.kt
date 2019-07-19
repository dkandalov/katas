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
        "ABCD".zigzag(rows = 4) shouldEqual "ABCD"
    }

    @Test fun `zigzag one cycle`() {
        // A     G
        // B   F
        // C E
        // D
        "ABCDEFG".zigzag(rows = 4) shouldEqual "AGBFCED"
    }

    @Test fun `zigzag two cycles`() {
        // A     G     N
        // B   F H   M
        // C E   J L
        // D     K
        "ABCDEFGHJKLMN".zigzag(rows = 4) shouldEqual "AGNBFHMCEJLDK"
    }
}

private fun String.zigzag(rows: Int = 3): String {
    val cycle = rows * 2 - 2

    val indices = if (rows == 3) {
        (0..(cycle/2)).map {
            listOf(it, cycle - it)
        }
//        listOf(listOf(0), listOf(1, 3), listOf(2))
    } else {
        listOf(listOf(0), listOf(1, 5), listOf(2, 4), listOf(3))
    }

    return indices.joinToString("") { indices ->
        mapIndexedNotNull { i, c ->
            if (indices.any { i % cycle == it }) c else null
        }.joinToString("")
    }
}
