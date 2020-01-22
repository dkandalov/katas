package katas.kotlin.leetcode.regex_matching.v4

import kotlincommon.test.shouldEqual
import org.junit.jupiter.api.Test

class RegexMatching {
    @Test fun `some examples`() {
        "".matchRegex("") shouldEqual true
    }
}

private fun String.matchRegex(regex: String): Boolean {
    return true
}
