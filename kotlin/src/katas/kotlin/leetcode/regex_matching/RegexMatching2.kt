package katas.kotlin.leetcode.regex_matching

import kotlincommon.test.*
import org.junit.*

class RegexMatching2 {
    @Test fun `some examples`() {
        match("a", "a") shouldEqual true
        match("aa", "a") shouldEqual false
    }
}

private fun match(input: String, regex: String): Boolean {
    return input == regex
}