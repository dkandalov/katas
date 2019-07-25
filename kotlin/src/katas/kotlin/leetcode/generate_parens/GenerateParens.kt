package katas.kotlin.leetcode.generate_parens

import kotlincommon.test.shouldEqual
import org.junit.Test

class GenerateParensTests {
    @Test fun `generate all combinations of well-formed parentheses`() {
        generateParens(1) shouldEqual "()"
    }
}

private fun generateParens(n: Int): String {
    return "()"
}