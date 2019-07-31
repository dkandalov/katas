package katas.kotlin.leetcode.validate_bst

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

class ValidateBSTTests {
    @Test fun `determine if binary search tree is valid`() {
        TreeNode(1).isValid() shouldEqual true
        TreeNode(1, TreeNode(0)).isValid() shouldEqual true
        TreeNode(1, null, TreeNode(2)).isValid() shouldEqual true

        TreeNode(1, TreeNode(2)).isValid() shouldEqual false
    }
}

private fun TreeNode.isValid(): Boolean {
    if (left != null && left!!.value > value) return false
    return true
}
