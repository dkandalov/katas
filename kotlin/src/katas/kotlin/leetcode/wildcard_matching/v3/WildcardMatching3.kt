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

//        match("", "*") shouldEqual true
        match("abc", "*") shouldEqual true
        match("abc", "a*") shouldEqual true
        match("abc", "*c") shouldEqual true
        match("abc", "a*c") shouldEqual true
        match("abc", "*a") shouldEqual false
        match("abc", "b*") shouldEqual false
    }
}

private fun match(s: String, pattern: String): Boolean {
    if (s.isEmpty() && pattern.isEmpty()) return true
    if (s.isEmpty() || pattern.isEmpty()) return false
    return when (pattern.first()) {
        '*'  -> (0..s.length)
            .map { s.substring(it, s.length) }
            .any { match(it, pattern.drop(1)) }
        '?'  -> match(s.drop(1), pattern.drop(1))
        else -> if (s.first() == pattern.first()) match(s.drop(1), pattern.drop(1)) else return false
    }
}