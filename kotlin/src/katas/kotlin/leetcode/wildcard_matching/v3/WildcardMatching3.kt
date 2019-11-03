package katas.kotlin.leetcode.wildcard_matching.v3

import kotlincommon.test.*
import org.junit.*

class WildcardMatching3 {
    @Test fun `some examples`() {
        match("", "") shouldEqual true
        match("", "a") shouldEqual false
        match("a", "") shouldEqual false

        match("a", "a") shouldEqual true
        match("a", "b") shouldEqual false
        match("a", "aa") shouldEqual false
        match("aa", "a") shouldEqual false

        match("", "?") shouldEqual false
        match("a", "?") shouldEqual true
        match("ab", "?") shouldEqual false
        match("ab", "??") shouldEqual true
        match("ab", "???") shouldEqual false
        match("abc", "??") shouldEqual false
    }
}

private fun match(s: String, pattern: String): Boolean {
    return s.length == pattern.length &&
        s.zip(pattern).all {
            it.second == '?' || it.first == it.second
        }
}