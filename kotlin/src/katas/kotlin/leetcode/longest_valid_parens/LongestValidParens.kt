package katas.kotlin.leetcode.longest_valid_parens

import datsok.shouldEqual
import org.junit.Test

class LongestValidParensTest {
    @Test fun examples() {
        longestValidParentheses("") shouldEqual ""
        longestValidParentheses(")") shouldEqual ""
        longestValidParentheses(")(") shouldEqual ""
        longestValidParentheses("()") shouldEqual "()"

        longestValidParentheses("(()") shouldEqual "()" // 1 2 1
        longestValidParentheses("())") shouldEqual "()" // 1 0 -1
        longestValidParentheses(")()(") shouldEqual "()" // -1 0 -1 1

        "(())".let {
            longestValidParentheses(")$it)") shouldEqual it // -1 0 1 0 -1 -2
            longestValidParentheses("())$it") shouldEqual it // 1 0 -1 0 1 0 -1
            longestValidParentheses("$it(()") shouldEqual it
        }

        "()()".let {
            longestValidParentheses(")$it)") shouldEqual it
            longestValidParentheses("())$it") shouldEqual it
            longestValidParentheses("$it(()") shouldEqual it
        }

        longestValidParentheses("))(())())(") shouldEqual "(())()"
    }
}

private fun longestValidParentheses(s: String): String {
    var result = ""
    (0..s.length).forEach { i ->
        (i..s.length).forEach { j ->
            val ss = s.substring(i, j)
            if (ss.hasValidParens() && ss.length > result.length) {
                result = ss
            }
        }
    }
    return result
}

private fun String.hasValidParens(): Boolean {
    var counter = 0
    forEach { char ->
        when (char) {
            '(' -> counter++
            ')' -> counter--
        }
        if (counter < 0) return false
    }
    return counter == 0
}
