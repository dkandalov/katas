package katas.kotlin.leetcode.string_to_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class StringToIntegerTests {
    @Test fun `0 to 100`() {
        (0 until 10).forEach { it.toString().toInteger() shouldEqual it }
        (10 until 100).forEach { it.toString().toInteger() shouldEqual it }
    }

    @Test fun `positive overflow`() {
        "1${Int.MAX_VALUE}".toInteger() shouldEqual Int.MAX_VALUE
    }

    @Test fun `negative overflow`() {
        "-1${Int.MAX_VALUE}".toInteger() shouldEqual Int.MIN_VALUE
    }

    @Test fun `-100 to 0`() {
        (-100 until 0).forEach { it.toString().toInteger() shouldEqual it }
    }

    @Test fun `invalid input`() {
        "-".toInteger() shouldEqual 0
        "--".toInteger() shouldEqual 0
        "+".toInteger() shouldEqual 0
        "++".toInteger() shouldEqual 0
        "-+".toInteger() shouldEqual 0
        "--123".toInteger() shouldEqual 0
    }
}

private fun String.toInteger(): Int {
    var result = 0
    var isNegative = false

    var i = 0
    val chars = toCharArray()
    fun hasNext() = i < chars.size
    fun next() = chars[i++]
    fun peek() = chars[i]

    val sign = if (hasNext()) peek() else -1
    if (sign == '-' || sign == '+') {
        isNegative = sign == '-'
        next()
    }

    while (hasNext()) {
        val char = next()
        if (char < '0' || char > '9') return 0
        val prevResult = result
        result = result * 10 + (char.toInt() - '0'.toInt())
        if (prevResult > result) return if (isNegative) Int.MIN_VALUE else Int.MAX_VALUE
    }

    if (isNegative) result = -result
    return result
}
