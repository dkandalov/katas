package katas.kotlin.leetcode.string_to_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class StringToIntegerTests {
    @Test fun `it works`() {
        (0 until 10).forEach {
            it.toString().toInteger() shouldEqual it
        }
        (10 until 100).forEach {
            it.toString().toInteger() shouldEqual it
        }
    }
}

private fun String.toInteger(): Int {
    var result = 0
    chars().forEach { char ->
        result = result * 10 + (char - '0'.toInt())
    }
    return result
}
