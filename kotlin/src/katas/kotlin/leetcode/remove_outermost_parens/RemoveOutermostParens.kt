package katas.kotlin.leetcode.remove_outermost_parens

import datsok.shouldEqual
import org.junit.Test

class RemoveOutermostParensTests {
    @Test fun `it mostly works`() {
        "()".removeOuterParens() shouldEqual ""
        "()()".removeOuterParens() shouldEqual ""
        "(())".removeOuterParens() shouldEqual "()"
        "(()())(())".removeOuterParens() shouldEqual "()()()"
        "(()())(())(()(()))".removeOuterParens() shouldEqual "()()()()(())"
    }
}

private fun String.removeOuterParens(): String {
    var result = ""
    var parenCount = 0
    toCharArray().forEach { char ->
        when (char) {
            '(' -> if (parenCount++ > 0) result += char
            ')' -> if (--parenCount > 0) result += char
        }
    }
    return result
}
