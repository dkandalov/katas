package katas.kotlin.leetcode.regex_matching.v3

import kotlincommon.test.shouldEqual
import org.junit.jupiter.api.Test

class RegexMatching {
    @Test fun `some examples`() {
        "".matchesRegex("") shouldEqual true

        "".matchesRegex("a") shouldEqual false
        "a".matchesRegex("") shouldEqual false
        "a".matchesRegex("a") shouldEqual true

        "a".matchesRegex(".") shouldEqual true
        "a".matchesRegex("..") shouldEqual false
    }
}

private fun String.matchesRegex(regex: String): Boolean {
    if (this.length != regex.length) return false
    return this.zip(regex).all { 
        it.first == it.second || it.second == '.'
    }
}
