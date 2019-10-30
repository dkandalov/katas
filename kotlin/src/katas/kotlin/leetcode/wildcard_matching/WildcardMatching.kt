package katas.kotlin.leetcode.wildcard_matching

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/wildcard-matching/
 *
 * Given an input string (s) and a pattern (p), implement wildcard pattern matching with support for '?' and '*'.
 * '?' Matches any single character.
 * '*' Matches any sequence of characters (including the empty sequence).
 *
 * The matching should cover the entire input string (not partial).
 *
 * Note:
 * s could be empty and contains only lowercase letters a-z.
 * p could be empty and contains only lowercase letters a-z, and characters like ? or *.
 */
class WildcardMatching {
    @Test
    fun `some examples`() {
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

    @Test
    fun `matchers examples`() {
        CharMatcher('a').invoke(Match("a")).any { it.isComplete } shouldEqual true
        SeqMatcher(listOf(CharMatcher('a'))).invoke(Match("a")).any { it.isComplete } shouldEqual true
        SeqMatcher(listOf(CharMatcher('a'))).invoke(Match("aa")).any { it.isComplete } shouldEqual false
    }
}

private data class Match(val s: String, val offset: Int = 0, val isComplete: Boolean = offset >= s.length)
private typealias Matcher = (match: Match) -> Sequence<Match>

private data class CharMatcher(val char: Char) : Matcher {
    override fun invoke(match: Match) =
        when {
            match.isComplete -> emptySequence()
            match.s[match.offset] == char -> sequenceOf(Match(match.s, match.offset + 1))
            else -> emptySequence()
        }
}

private data class SeqMatcher(val matchers: List<Matcher>) : Matcher {
    override fun invoke(match: Match) =
        matchers.fold(sequenceOf(match)) { matches, matcher ->
            matches.flatMap { matcher.invoke(it) }
        }
}

private class QuestionMatcher : Matcher {
    override fun invoke(match: Match) =
        if (match.isComplete) emptySequence()
        else sequenceOf(Match(match.s, match.offset + 1))
}

private class StarMatcher : Matcher {
    override fun invoke(match: Match) =
        (match.offset..match.s.length).asSequence().map { offset ->
            Match(match.s, offset)
        }
}

private fun isMatch(s: String, pattern: String): Boolean {
    val matcher = SeqMatcher(pattern.map {
        when (it) {
            '*' -> StarMatcher()
            '?' -> QuestionMatcher()
            else -> CharMatcher(it)
        }
    })
    return matcher.invoke(Match(s)).any { match -> match.isComplete }
}
