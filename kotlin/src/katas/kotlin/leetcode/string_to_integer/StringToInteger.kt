package katas.kotlin.leetcode.string_to_integer

import kotlincommon.test.shouldEqual
import org.junit.Test

class StringToIntegerTests {
    @Test fun `it works`() {
        "0".toInteger() shouldEqual 0
        "1".toInteger() shouldEqual 1
    }
}

private fun String.toInteger(): Int {
    return first().toInt() - '0'.toInt()
}
