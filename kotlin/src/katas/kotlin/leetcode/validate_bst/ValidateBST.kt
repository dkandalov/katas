package katas.kotlin.leetcode.validate_bst

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

class ValidateBSTTests {
    @Test fun `determine if binary search tree is valid`() {
        TreeNode(1).isValid() shouldEqual true
    }
}

private fun TreeNode.isValid(): Boolean {
    return true
}
