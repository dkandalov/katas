package katas.kotlin.leetcode.wildcard_matching.v4

import datsok.shouldEqual
import org.junit.Test

typealias Matcher = (String) -> Set<String>

fun char(char: Char): Matcher = { s ->
    if (s.firstOrNull() == char) setOf(s.drop(1)) else emptySet()
}

fun anyChar(): Matcher = { s ->
    if (s.isNotEmpty()) setOf(s.drop(1)) else emptySet()
}

fun anyChars(): Matcher = { s ->
    (0..s.length).mapTo(HashSet()) { s.drop(it) }
}

fun isMatch(s: String, pattern: String): Boolean {
    val matchers = pattern.map {
        when (it) {
            '?' -> anyChar()
            '*' -> anyChars()
            else -> char(it)
        }
    }
    return matchers
        .fold(setOf(s)) { outputs, matcher -> outputs.flatMapTo(HashSet()) { matcher(it) } }
        .any { output -> output.isEmpty() }
}

// ✅
class WildcardMatching {
    @Test fun `some examples`() {
        isMatch("a", "a") shouldEqual true
        isMatch("a", "b") shouldEqual false

        isMatch("aa", "aa") shouldEqual true
        isMatch("aa", "a") shouldEqual false
        isMatch("a", "aa") shouldEqual false

        isMatch("", "*") shouldEqual true
        isMatch("a", "*") shouldEqual true
        isMatch("aa", "*") shouldEqual true
        isMatch("aa", "**") shouldEqual true
        isMatch("aa", "***") shouldEqual true
        isMatch("aa", "*a") shouldEqual true
        isMatch("aa", "a*") shouldEqual true
        isMatch("aa", "*aa") shouldEqual true
        isMatch("aa", "a*a") shouldEqual true
        isMatch("aa", "aa*") shouldEqual true
        isMatch("aa", "*b") shouldEqual false
        isMatch("aa", "b*") shouldEqual false
        isMatch("adceb", "*a*b") shouldEqual true

        isMatch("a", "?") shouldEqual true
        isMatch("cb", "?a") shouldEqual false
        isMatch("acdcb", "a*c?b") shouldEqual false
    }
}
