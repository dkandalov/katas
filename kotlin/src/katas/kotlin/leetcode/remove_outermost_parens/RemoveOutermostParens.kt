package katas.kotlin.leetcode.remove_outermost_parens

import kotlincommon.test.shouldEqual
import org.junit.Test

class RemoveOutermostParensTests {
    @Test fun `fooo`() {
        "()".removeOuterParens() shouldEqual ""
        "()()".removeOuterParens() shouldEqual ""
    }
}

private fun String.removeOuterParens(): String {
    return ""
}
