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

//        "".matchRegex("a?") shouldEqual true
//        "a".matchRegex("a?") shouldEqual true
    }
}

typealias Matcher = (String) -> List<String>

fun char(c: Char): Matcher = { input ->
    if (input.first() == c) listOf(input.drop(1)) else emptyList()
}

fun anyChar(): Matcher = { input ->
    if (input.isNotEmpty()) listOf(input.drop(1)) else emptyList()
}

private fun String.matchRegex(regex: String): Boolean {
    val matchers = regex.fold(emptyList<Matcher>()) { acc, c ->
        acc + (if (c == '.') anyChar() else char(c))
    }
    return matchers
        .fold(listOf(this)) { inputs, matcher ->
            inputs.flatMap(matcher).distinct()
        }
        .any { it.isEmpty() }
}
