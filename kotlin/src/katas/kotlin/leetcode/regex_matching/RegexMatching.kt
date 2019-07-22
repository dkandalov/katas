package katas.kotlin.leetcode.regex_matching

import kotlincommon.test.shouldEqual
import org.junit.Test

class RegexMatchingTests {
    @Test fun `it mostly works`() {
        "".matches("") shouldEqual true
    }
}

private fun String.matches(regex: String): Boolean {
    return true
}
