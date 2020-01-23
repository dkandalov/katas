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
    }
}

private fun String.matchesRegex(regex: String): Boolean {
    return this == regex
}
