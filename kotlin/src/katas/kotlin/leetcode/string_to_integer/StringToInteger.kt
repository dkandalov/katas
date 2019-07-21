package katas.kotlin.leetcode.string_to_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class StringToIntegerTests {
    @Test fun `it works`() {
        (0 until 10).forEach {
            it.toString().toInteger() shouldEqual it
        }
        (10 until 20).forEach {
            it.toString().toInteger() shouldEqual it
        }
        "0".toInteger() shouldEqual 0
        "1".toInteger() shouldEqual 1
    }
}

private fun String.toInteger(): Int {
    var result = 0
    chars().forEach { char ->
        result = result * 10 + (char - '0'.toInt())
    }
    return result
}
