package katas.kotlin.leetcode.regex_matching.v2

import datsok.*
import org.junit.*

class RegexMatching {
    @Test fun `some examples`() {
        match("", "") shouldEqual true
        match("a", "a") shouldEqual true
        match("a", "aa") shouldEqual false
        match("aa", "a") shouldEqual false

        match("aa", "a*") shouldEqual true
        match("aaa", "a*") shouldEqual true
        match("ba", "a*") shouldEqual false

        match("a", ".") shouldEqual true
        match("ab", ".") shouldEqual false
        match("ab", "..") shouldEqual true
        match("ab", "b.") shouldEqual false
        match("abc", "...") shouldEqual true
        match("abc", ".bc") shouldEqual true
        match("abc", "a.c") shouldEqual true
        match("abc", "ab.") shouldEqual true
        match("abc", ".bc") shouldEqual true

        match("abcdef", ".*") shouldEqual true
        match("aab", "c*a*b") shouldEqual true
        match("mississippi", "mis*is*p*.") shouldEqual false
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
            .flatMap { output -> zeroOrMore(matcher)(output) }
            .forEach { output -> if (output != input) yield(output) }
    }.toList()
}

private fun anyChar(): Matcher = { input ->
    if (input.isEmpty()) emptyList() else listOf(input.drop(1))
}

private fun match(input: String, regex: String): Boolean {
    val matchers = regex.toCharArray().fold(emptyList<Matcher>()) { matchers, char ->
        when (char) {
            '.'  -> matchers + anyChar()
            '*'  -> matchers.dropLast(1) + zeroOrMore(matchers.last())
            else -> matchers + charMatcher(char)
        }
    }
    val regexMatcher = sequenceMatcher(matchers)
    return regexMatcher(input).any { it.isEmpty() }
}