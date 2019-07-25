package katas.kotlin.leetcode.generate_parens

import kotlincommon.test.shouldEqual
import org.junit.Test

class GenerateParensTests {
    @Test fun `generate all combinations of well-formed parentheses`() {
        generateParens(1) shouldEqual listOf("()")
        generateParens(2) shouldEqual listOf("()()", "(())")
        generateParens(3) shouldEqual listOf(
            "()()()", "(()())", "()(())", "(())()", "((()))"
        )
    }
}

private fun generateParens(n: Int): List<String> {
    if (n == 1) return listOf("()")
    return generateParens(n - 1).flatMap {
        listOf("()$it", "$it()", "($it)")
    }.distinct()
}