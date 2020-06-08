package katas.kotlin.leetcode.regex_matching.v3

import datsok.shouldEqual
import org.junit.jupiter.api.Test

class RegexMatching {
    @Test fun `some examples`() {
        "".matchesRegex("") shouldEqual true

        "".matchesRegex("a") shouldEqual false
        "a".matchesRegex("") shouldEqual false
        "a".matchesRegex("a") shouldEqual true

        "a".matchesRegex(".") shouldEqual true
        "a".matchesRegex("..") shouldEqual false
        "ab".matchesRegex("..") shouldEqual true
        "ab".matchesRegex("a.") shouldEqual true
        "ab".matchesRegex(".b") shouldEqual true

        "a".matchesRegex("a?") shouldEqual true
        "a".matchesRegex("a?b?") shouldEqual true
        "b".matchesRegex("a?b?") shouldEqual true
        "c".matchesRegex("a?b?") shouldEqual false

        "".matchesRegex("a*") shouldEqual true
        "a".matchesRegex("a*") shouldEqual true
        "b".matchesRegex("a*") shouldEqual false
        "aa".matchesRegex("a*") shouldEqual true
        "aab".matchesRegex("a*") shouldEqual false
        "aab".matchesRegex("a*b") shouldEqual true
        "baa".matchesRegex("ba*") shouldEqual true

        "abc".matchesRegex(".*") shouldEqual true
    }
}

typealias Matcher = (String) -> List<String>

fun char(c: Char): Matcher = { input ->
    if (input.firstOrNull() == c) listOf(input.drop(1)) else emptyList()
}

fun anyChar(): Matcher = { input ->
    if (input.isNotEmpty()) listOf(input.drop(1)) else emptyList()
}

fun optional(matcher: Matcher): Matcher = { input ->
    listOf(input) + matcher(input)
}

fun zeroOrMore(matcher: Matcher): Matcher = { input ->
    listOf(input) + matcher(input).flatMap(zeroOrMore(matcher))
}

private fun String.matchesRegex(regex: String): Boolean {
    val matchers = regex.fold(emptyList<Matcher>()) { matchers, c ->
        when (c) {
            '.'  -> matchers + anyChar()
            '?'  -> matchers.dropLast(1) + optional(matchers.last())
            '*'  -> matchers.dropLast(1) + zeroOrMore(matchers.last())
            else -> matchers + char(c)
        }
    }
    return matchers
        .fold(listOf(this)) { inputs, matcher -> inputs.flatMap(matcher) }
        .any { it.isEmpty() }
}
