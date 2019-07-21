package katas.kotlin.leetcode.string_to_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class StringToIntegerTests {
    @Test fun `0 to 100`() {
        (0 until 100).forEach { it.toString().toInteger() shouldEqual it }
        (0 until 100).forEach { ("+$it").toInteger() shouldEqual it }
    }

    @Test fun `-100 to 0`() {
        (-100 until 0).forEach { it.toString().toInteger() shouldEqual it }
    }

    @Test fun `ignore trailing non-digits`() {
        "123foo".toInteger() shouldEqual 123
        "123 234".toInteger() shouldEqual 123
    }

    @Test fun `ignore prefix spaces`() {
        "  123".toInteger() shouldEqual 123
        " -123".toInteger() shouldEqual -123
    }

    @Test fun `positive overflow`() {
        "1${Int.MAX_VALUE}".toInteger() shouldEqual Int.MAX_VALUE
    }

    @Test fun `negative overflow`() {
        "-1${Int.MAX_VALUE}".toInteger() shouldEqual Int.MIN_VALUE
    }

    @Test fun `invalid input`() {
        "-".toInteger() shouldEqual 0
        "--".toInteger() shouldEqual 0
        "+".toInteger() shouldEqual 0
        "++".toInteger() shouldEqual 0
        "-+".toInteger() shouldEqual 0
        "--123".toInteger() shouldEqual 0
        "- 123".toInteger() shouldEqual 0
        "foo123".toInteger() shouldEqual 0
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

    while (hasNext() && peek() == ' ') next()

    val sign = if (hasNext()) peek() else -1
    if (sign == '-' || sign == '+') {
        isNegative = sign == '-'
        next()
    }

    var hadOneDigit = false
    while (hasNext()) {
        val char = next()
        if (char < '0' || char > '9') return if (hadOneDigit) result else 0
        hadOneDigit = true

        val prevResult = result
        result = result * 10 + (char.toInt() - '0'.toInt())
        if (prevResult > result) return if (isNegative) Int.MIN_VALUE else Int.MAX_VALUE
    }

    if (isNegative) result = -result
    return result
}
