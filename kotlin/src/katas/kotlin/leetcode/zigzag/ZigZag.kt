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
    val indices = (0..(cycle/2)).map { Pair(it, cycle - it) }

    return indices.joinToString("") { (first, second) ->
        mapIndexedNotNull { i, c ->
            if (i % cycle == first || i % cycle == second) c else null
        }.joinToString("")
    }
}
