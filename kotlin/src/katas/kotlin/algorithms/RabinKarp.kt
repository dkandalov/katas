package katas.kotlin.algorithms

import katas.kotlin.shouldEqual
import org.junit.Test

class RollingHash(
    val s: CharSequence,
    val size: Int,
    val startIndex: Int = 0,
    val base: Int = 256,
    val modulus: Int = 101,
    val value: Int = s.subSequence(startIndex, startIndex + size).fold(0) { acc, c ->
        (acc * base + c.toInt()).rem(modulus)
    }
) {
    fun roll(): RollingHash {
        if (startIndex + size >= s.length) return this

        var prevMultiplier = 1
        1.until(size).forEach {
            prevMultiplier = (prevMultiplier * base).rem(modulus)
        }

        val prevChar = s[startIndex]
        val newChar = s[startIndex + size]
        var newValue = value + modulus - (prevChar.toInt() * prevMultiplier) % modulus
        newValue = (newValue * base + newChar.toInt()).rem(modulus)

        return RollingHash(s, size, startIndex + 1, base, modulus, newValue)
    }
}

fun findIndexOf(s: String, text: String): Int {
    if (text.length < s.length) return -1

    val hash = RollingHash(s, s.length)
    var textHash = RollingHash(text, s.length)

    0.until(text.length - s.length + 1).forEach { i ->
        if (hash.value == textHash.value) return i
        textHash = textHash.roll()
    }

    return -1
}

class RobinKarpTests {
    @Test fun `find index of a string in text`() {
        findIndexOf("", text = "") shouldEqual 0
        findIndexOf("", text = "abc") shouldEqual 0
        findIndexOf("abc", text = "") shouldEqual -1
        findIndexOf("abc", text = "abc") shouldEqual 0

        findIndexOf("ab", "abcdef") shouldEqual 0
        findIndexOf("cd", "abcdef") shouldEqual 2
        findIndexOf("ef", "abcdef") shouldEqual 4
    }

    // https://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm
    @Test fun `rolling hash example from wikipedia`() {
        val hash = RollingHash("abra", size = 3)
        hash.value shouldEqual 4
        hash.roll().value shouldEqual 30
        RollingHash("abra", startIndex = 1, size = 3).value shouldEqual 30
    }

    @Test fun `rolling hash`() {
        val hash = RollingHash("hihi", size = 2)
        hash.value shouldEqual 65
        hash.roll().value shouldEqual 17
        hash.roll().roll().value shouldEqual 65
        hash.roll().roll().roll().value shouldEqual 65
    }
}