package katas.kotlin.leetcode.regex_matching.v3

import kotlincommon.test.shouldEqual
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

        //"a".matchesRegex("a?") shouldEqual true
        //"a".matchesRegex("a?b?") shouldEqual true
        //"b".matchesRegex("a?b?") shouldEqual true
        //"c".matchesRegex("a?b?") shouldEqual false
    }
}

typealias Matcher = (String) -> List<String>

fun char(c: Char): Matcher = { input ->
    if (input.firstOrNull() == c) listOf(input.drop(1)) else emptyList()
}

fun anyChar(): Matcher = { input ->
    if (input.isNotEmpty()) listOf(input.drop(1)) else emptyList()
}

private fun String.matchesRegex(regex: String): Boolean {
    val matchers = regex.map { if (it == '.') anyChar() else char(it) }
    return matchers
        .fold(listOf(this)) { inputs, matcher ->
            inputs.flatMap(matcher)
        }
        .any { it.isEmpty() }
}
