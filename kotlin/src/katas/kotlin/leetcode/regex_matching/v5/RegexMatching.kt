package katas.kotlin.leetcode.regex_matching.v5

import kotlincommon.test.shouldEqual
import org.junit.jupiter.api.Test

class RegexMatching {
    @Test fun `some examples`() {
        "".matchesRegex("") shouldEqual true
        "".matchesRegex("a") shouldEqual false
        "a".matchesRegex("") shouldEqual false
        "a".matchesRegex("a") shouldEqual true
        "ab".matchesRegex("ab") shouldEqual true

        "a".matchesRegex(".") shouldEqual true
        "b".matchesRegex(".") shouldEqual true
        "".matchesRegex(".") shouldEqual false
        "ab".matchesRegex("a.") shouldEqual true
        "ab".matchesRegex(".b") shouldEqual true
        "ab".matchesRegex("c.") shouldEqual false
    }
}

private fun String.matchesRegex(regex: String): Boolean {
    if (this.length != regex.length) return false
    return this.zip(regex)
        .all { it.second == '.' || it.first == it.second }
}
