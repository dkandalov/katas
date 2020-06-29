package katas.kotlin.leetcode.generate_parens

import katas.kotlin.softFail
import org.junit.jupiter.api.Test

abstract class GenerateParensTests(val generate: (Int) -> List<String>) {
    @Test fun `it works`() = softFail {
        generate(1) shouldEqual listOf("()")
        generate(2) shouldEqual listOf("()()", "(())")
        generate(3) shouldEqual listOf(
            "()()()",
            "(()())",
            "()(())",
            "(())()",
            "((()))"
        )
        generate(4) shouldEqual listOf(
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

class RecursiveParensTests : GenerateParensTests(::parensRecur)

private fun parensRecur(n: Int): List<String> {
    return when (n) {
        0    -> listOf("")
        1    -> listOf("()")
        else -> parensRecur(n - 1).flatMap {
            listOf("()$it", "$it()", "($it)").distinct()
        }
    }
}

class IterativeParensTests : GenerateParensTests(::parens)

private fun parens(n: Int): List<String> {
    var counter = n
    var result = listOf("")
    while (counter > 0) {
        result = result.flatMap { listOf("()$it", "$it()", "($it)").distinct() }
        counter--
    }
    return result
}
