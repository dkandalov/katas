package katas.kotlin.leetcode.regex_matching.v3

import org.junit.jupiter.api.Test

class RegexMatching {
    @Test fun `some examples`() {
        "".matchesRegex("")
    }
}

private fun String.matchesRegex(regex: String): Boolean {
    return true
}
