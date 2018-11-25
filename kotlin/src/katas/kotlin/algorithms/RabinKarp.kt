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

        val prevChar = s[startIndex].toInt()
        val newChar = s[startIndex + size]
        var newValue = value + modulus - (prevChar * prevMultiplier) % modulus
        newValue = (newValue * base + newChar.toInt()).rem(modulus)

        return RollingHash(s, size, startIndex + 1, base, modulus, newValue)
    }
}

class RollingHashTests {
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