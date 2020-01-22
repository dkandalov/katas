package katas.kotlin.leetcode.regex_matching.v4

import kotlincommon.test.shouldEqual
import org.junit.jupiter.api.Test

class RegexMatching {
    @Test fun `some examples`() {
        "".matchRegex("") shouldEqual true
        "a".matchRegex("a") shouldEqual true
        "ab".matchRegex("ab") shouldEqual true
        "a".matchRegex("b") shouldEqual false
        "ab".matchRegex("a") shouldEqual false

        "a".matchRegex(".") shouldEqual true
        "ab".matchRegex("..") shouldEqual true
        "ab".matchRegex("a.") shouldEqual true
        "ab".matchRegex(".b") shouldEqual true
        "abc".matchRegex("..") shouldEqual false
        "abc".matchRegex("b..") shouldEqual false
    }
}

private fun String.matchRegex(regex: String): Boolean {
    if (this.length != regex.length) return false
    return this.zip(regex).all { it.first == it.second || it.second == '.' }
}
