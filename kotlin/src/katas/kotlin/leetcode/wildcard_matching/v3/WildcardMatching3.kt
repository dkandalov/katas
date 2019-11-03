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

        match("", "*") shouldEqual true
        match("abc", "*") shouldEqual true
        match("abc", "a*") shouldEqual true
        match("abc", "*c") shouldEqual true
        match("abc", "a*c") shouldEqual true
        match("abc", "*a") shouldEqual false
        match("abc", "b*") shouldEqual false
        match("abc", "****") shouldEqual true
    }
}

private typealias Matcher = (input: String) -> List<String>

private fun char(c: Char): Matcher = { input ->
    if (input.isEmpty() || input.first() != c) emptyList()
    else listOf(input.drop(1))
}

private fun questionMark(): Matcher = { input ->
    if (input.isEmpty()) emptyList()
    else listOf(input.drop(1))
}

private fun star(): Matcher = { input ->
    (0..input.length).map { input.substring(it, input.length) }
}

private fun match(s: String, pattern: String): Boolean {
    if (s.isEmpty() && pattern.isEmpty()) return true
    if (pattern.isEmpty()) return false

    val matchers = pattern.map {
        when (it) {
            '*'  -> star()
            '?'  -> questionMark()
            else -> char(it)
        }
    }
    return matchers
        .fold(listOf(s)) { inputs, matcher -> inputs.flatMap { matcher(it) } }
        .any { input -> input.isEmpty() }
}