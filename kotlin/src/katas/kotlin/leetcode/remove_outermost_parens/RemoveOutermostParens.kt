package katas.kotlin.leetcode.remove_outermost_parens

import kotlincommon.test.shouldEqual
import org.junit.Test

class RemoveOutermostParensTests {
    @Test fun `fooo`() {
        "()".removeOuterParens() shouldEqual ""
        "()()".removeOuterParens() shouldEqual ""
        "(())".removeOuterParens() shouldEqual "()"
    }
}

private fun String.removeOuterParens(): String {
    var result = ""
    var outerParen = false
    toCharArray().forEach { char ->
        when (char) {
            '(' -> if (outerParen) result += char else outerParen = true
            ')' -> if (outerParen) outerParen = false else result += char
        }
    }
    return result
}
