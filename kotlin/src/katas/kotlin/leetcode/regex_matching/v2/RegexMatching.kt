package katas.kotlin.leetcode.regex_matching.v2

import kotlincommon.test.*
import org.junit.*

class RegexMatching {
    @Test fun `some examples`() {
        match("", "") shouldEqual true
        match("a", "a") shouldEqual true
        match("a", "aa") shouldEqual false
        match("aa", "a") shouldEqual false

        match("aa", "a*") shouldEqual true
        match("aaa", "a*") shouldEqual true
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

private fun zeroOrMore(matcher: Matcher): Matcher = { input ->
    sequence {
        yield(input)
        matcher(input)
            .flatMap { zeroOrMore(matcher)(it) }
            .forEach { if (it != input) yield(it) }
    }.toList()
}

private fun match(input: String, regex: String): Boolean {
    val matchers = regex.toCharArray().fold(emptyList<Matcher>()) { matchers, char ->
        when {
            matchers.isEmpty() -> listOf(charMatcher(char))
            char == '*'        -> matchers + zeroOrMore(matchers.last())
            else               -> matchers + charMatcher(char)
        }
    }
    val regexMatcher = sequenceMatcher(matchers)
    return regexMatcher(input).any { it.isEmpty() }
}