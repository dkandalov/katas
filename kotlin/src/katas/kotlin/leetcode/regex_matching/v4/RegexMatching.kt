package katas.kotlin.leetcode.regex_matching.v4

import kotlincommon.test.shouldEqual
import org.junit.jupiter.api.Test

class RegexMatching {
    @Test fun `some examples`() {
        "".matchRegex("") shouldEqual true
        "a".matchRegex("a") shouldEqual true
        "ab".matchRegex("ab") shouldEqual true
        "a".matchRegex("b") shouldEqual false
        "ab".matchRegex("a") shouldEqual false

        "a".matchRegex(".") shouldEqual true
        "ab".matchRegex("..") shouldEqual true
        "ab".matchRegex("a.") shouldEqual true
        "ab".matchRegex(".b") shouldEqual true
        "abc".matchRegex("..") shouldEqual false
        "abc".matchRegex("b..") shouldEqual false

        "".matchRegex("a?") shouldEqual true
        "a".matchRegex("a?") shouldEqual true
        "b".matchRegex("a?") shouldEqual false

        "".matchRegex("a*") shouldEqual true
        "a".matchRegex("a*") shouldEqual true
        "aaa".matchRegex("a*") shouldEqual true
        "b".matchRegex("a*") shouldEqual false
    }
}

typealias Matcher = (String) -> List<String>

fun char(c: Char): Matcher = { input ->
    if (input.firstOrNull() == c) listOf(input.drop(1)) else emptyList()
}

fun anyChar(): Matcher = { input ->
    if (input.isNotEmpty()) listOf(input.drop(1)) else emptyList()
}

fun zeroOrOne(matcher: Matcher): Matcher = { input ->
    listOf(input) + matcher(input)
}

fun zeroOrMore(matcher: Matcher): Matcher = { input ->
    listOf(input) + matcher(input).flatMap(zeroOrMore(matcher))
}

private fun String.matchRegex(regex: String): Boolean {
    val matchers = regex.fold(emptyList<Matcher>()) { acc, c ->
        when (c) {
            '.'  -> acc + anyChar()
            '?'  -> acc.dropLast(1) + zeroOrOne(acc.last())
            '*'  -> acc.dropLast(1) + zeroOrMore(acc.last())
            else -> acc + char(c)
        }
    }
    return matchers
        .fold(listOf(this)) { inputs, matcher ->
            inputs.flatMap(matcher)
        }
        .any { it.isEmpty() }
}
