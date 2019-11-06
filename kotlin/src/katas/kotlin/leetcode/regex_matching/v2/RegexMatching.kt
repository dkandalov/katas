package katas.kotlin.leetcode.regex_matching.v2

import kotlincommon.test.*
import org.junit.*

class RegexMatching {
    @Test fun `some examples`() {
        match("", "") shouldEqual true
        match("a", "a") shouldEqual true
        match("a", "aa") shouldEqual false
        match("aa", "a") shouldEqual false

        // match("aa", "a*") shouldEqual true
    }
}

private typealias Matcher = (String) -> List<String>

private fun charMatcher(char: Char): Matcher = { input ->
    if (input.isEmpty() || input.first() != char) emptyList()
    else listOf(input.drop(1))
}

private fun sequenceMatcher(matchers: List<Matcher>): Matcher = { input ->
    matchers.fold(listOf(input)) { inputs, matcher ->
        inputs.flatMap(matcher)
    }
}

private fun zeroOrMore(char: Char): Matcher = { input ->
    sequence {
        yield(input)
        if (input.first() == char) yield(input.drop(1))
    }.toList()
}

private fun match(input: String, regex: String): Boolean {
    val matchers = regex.toCharArray().fold(emptyList<Matcher>()) { matchers, char ->
        if (matchers.isEmpty()) listOf(charMatcher(char))
        else matchers + charMatcher(char)
    }
    val regexMatcher = sequenceMatcher(matchers)
    return regexMatcher(input).any { it.isEmpty() }
}