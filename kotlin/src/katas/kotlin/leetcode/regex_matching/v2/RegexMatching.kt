package katas.kotlin.leetcode.regex_matching.v2

import kotlincommon.test.*
import org.junit.*

class RegexMatching {
    @Test fun `some examples`() {
        match("", "") shouldEqual true
        match("a", "a") shouldEqual true
        match("a", "aa") shouldEqual false
        match("aa", "a") shouldEqual false

        // match("aa", "a*") shouldEqual true
    }
}

private typealias Matcher = (String) -> String?

private fun str(char: Char): Matcher = { input ->
    if (input.isEmpty() || input.first() != char) null
    else input.drop(1)
}

private fun match(input: String, regex: String): Boolean {
    return input == regex
}