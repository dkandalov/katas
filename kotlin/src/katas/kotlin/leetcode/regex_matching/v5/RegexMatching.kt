package katas.kotlin.leetcode.regex_matching.v5

import kotlincommon.test.shouldEqual
import org.junit.jupiter.api.Test

class RegexMatching {
    @Test fun `some examples`() {
        "".matchesRegex("") shouldEqual true
    }
}

private fun String.matchesRegex(regex: String): Boolean {
    return true
}
