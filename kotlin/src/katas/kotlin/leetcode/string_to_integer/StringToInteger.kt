package katas.kotlin.leetcode.string_to_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class StringToIntegerTests {
    @Test fun `0 to 100`() {
        (0 until 10).forEach { it.toString().toInteger() shouldEqual it }
        (10 until 100).forEach { it.toString().toInteger() shouldEqual it }
    }

    @Test fun `-100 to 0`() {
        (-100 until 0).forEach { it.toString().toInteger() shouldEqual it }
    }

    @Test fun `invalid input`() {
        "-".toInteger() shouldEqual 0
        "--".toInteger() shouldEqual 0
    }
}

private fun String.toInteger(): Int {
    var result = 0
    var isNegative = false
    var 

    chars().forEach { char ->
        if (char == '-'.toInt()) isNegative = true
        else result = result * 10 + (char - '0'.toInt())
    }
    if (isNegative) result = -result
    return result
}
