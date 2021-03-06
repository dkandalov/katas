package katas.kotlin.leetcode.generate_parens

import datsok.shouldEqual
import org.junit.Test

class GenerateParens {
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
    return generateParens(n - 1).mapIndexed { i, it ->
        if (i == 0) listOf("()$it", "($it)")
        else listOf("()$it", "$it()", "($it)")
    }.flatten()
}