package katas.kotlin.leetcode.regex_matching

import kotlincommon.test.shouldEqual
import org.junit.Test

class RegexMatchingTests {
    @Test fun `it mostly works`() {
        "".matches("") shouldEqual true
        "a".matches("a") shouldEqual true
        "a".matches(".") shouldEqual true
        "a".matches("b") shouldEqual false
    }
}

private fun String.matches(regex: String): Boolean {
    if (regex == ".") return true
    return this == regex
}
