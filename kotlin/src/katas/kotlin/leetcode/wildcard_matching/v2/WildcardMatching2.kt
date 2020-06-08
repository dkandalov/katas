package katas.kotlin.leetcode.wildcard_matching.v2

import datsok.*
import org.junit.*

class WildcardMatching2 {
    @Test fun `some examples`() {
        match("", "") shouldEqual true
        match("a", "") shouldEqual false
        match("", "a") shouldEqual false

        match("a", "a") shouldEqual true
        match("a", "b") shouldEqual false
        match("aa", "a") shouldEqual false
        match("a", "aa") shouldEqual false
        match("ab", "ab") shouldEqual true

        match("", "?") shouldEqual false
        match("a", "?") shouldEqual true
        match("?", "a") shouldEqual false
        match("?", "?") shouldEqual true
        match("ab", "a?") shouldEqual true
        match("ab", "??") shouldEqual true
        match("ab", "???") shouldEqual false
        match("abc", "??") shouldEqual false

        match("", "*") shouldEqual true
        match("abc", "*") shouldEqual true
        match("abc", "ab*") shouldEqual true
        match("abc", "a*c") shouldEqual true
        match("abc", "*bc") shouldEqual true
        match("abc", "a*") shouldEqual true
        match("abc", "*c") shouldEqual true
        match("abc", "*cc") shouldEqual false
        match("abc", "aa*") shouldEqual false
        match("abc", "**") shouldEqual true
    }
}

private typealias Matcher = (input: String) -> List<String>

private fun char(c: Char): Matcher = { input: String ->
    if (input.isEmpty() || input.first() != c) emptyList()
    else listOf(input.drop(1))
}

private fun question(): Matcher = { input: String ->
    if (input.isEmpty()) emptyList()
    else listOf(input.drop(1))
}

private fun star(): Matcher = { input: String ->
    if (input.isEmpty()) listOf(input)
    else input.indices.map { i -> input.substring(i + 1, input.length) }
}

private fun match(s: String, pattern: String): Boolean {
    val matchers = pattern.map {
        when (it) {
            '?'  -> question()
            '*'  -> star()
            else -> char(it)
        }
    }
    return matchers
        .fold(listOf(s)) { inputs, matcher -> inputs.flatMap(matcher) }
        .any { it.isEmpty() }
}