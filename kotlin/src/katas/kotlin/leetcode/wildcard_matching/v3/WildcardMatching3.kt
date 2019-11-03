package katas.kotlin.leetcode.wildcard_matching.v3

import kotlincommon.test.*
import org.junit.*

class WildcardMatching3 {
    @Test fun `some examples`() {
        match("abc", "a") shouldEqual false
        match("abc", "abc") shouldEqual true
    }
}

private fun match(s: String, pattern: String): Boolean {
    return s == pattern
}