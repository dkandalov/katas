package katas.kotlin.leetcode.generate_parens

import datsok.shouldEqual
import org.junit.jupiter.api.Test

class GenerateParens2 {
    @Test fun `it works`() {
        parens(n = 1) shouldEqual listOf("()")
        parens(n = 2) shouldEqual listOf("()()", "(())")
        parens(n = 3) shouldEqual listOf(
            "()()()",
            "(()())",
            "()(())",
            "(())()",
            "((()))"
        )
        parens(n = 4) shouldEqual listOf(
            "()()()()",
            "(()()())",
            "()(()())",
            "(()())()",
            "((()()))",
            "()()(())",
            "()(())()",
            "(()(()))",
            "()(())()",
            "(())()()",
            "((())())",
            "()((()))",
            "((()))()",
            "(((())))"
        )
    }
}

private fun parens(n: Int): List<String> {
    return when (n) {
        0    -> listOf("")
        1    -> listOf("()")
        else -> parens(n - 1).flatMap {
            listOf("()$it", "$it()", "($it)").distinct()
        }
    }
}
