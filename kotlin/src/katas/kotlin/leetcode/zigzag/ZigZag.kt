package katas.kotlin.leetcode.zigzag

import nonstdlib.printed
import datsok.shouldEqual
import org.junit.Test
import kotlin.system.measureTimeMillis

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

class ZigZagLongStringTests {
    @Test fun `long string`() {
        generateString(7).zigzag() shouldEqual "aebdfhcg"
        measureTimeMillis {
            longString.zigzag()
        }.printed()
    }

    private val longString = generateString(10_000_000)

    private fun generateString(length: Int) = (0..length).map { (it + 'a'.toInt()).toChar() }.joinToString("")
}

private fun String.zigzag(rows: Int = 3): String {
    val cycle = rows * 2 - 2
    return (0..(cycle/2)).joinToString("") {
        val first = it
        val second = cycle - it
        filterIndexed { i, _ -> i % cycle == first || i % cycle == second }
    }
}
