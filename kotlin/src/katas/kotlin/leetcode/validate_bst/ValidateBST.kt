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
        TreeNode(1, null, TreeNode(0)).isValid() shouldEqual false

        TreeNode(1, TreeNode(1)).isValid() shouldEqual true
        TreeNode(1, null, TreeNode(1)).isValid() shouldEqual false

        TreeNode(4,
            left = TreeNode(2, TreeNode(1), TreeNode(3)),
            right = TreeNode(6)
        ).isValid() shouldEqual true
        TreeNode(4,
            left = TreeNode(2, TreeNode(3), TreeNode(1)),
            right = TreeNode(6)
        ).isValid() shouldEqual false

        TreeNode(4,
            left = TreeNode(2),
            right = TreeNode(6, TreeNode(5), TreeNode(7))
        ).isValid() shouldEqual true
        TreeNode(4,
            left = TreeNode(2),
            right = TreeNode(6, TreeNode(7), TreeNode(5))
        ).isValid() shouldEqual false

        TreeNode(4,
            left = TreeNode(2),
            right = TreeNode(6, TreeNode(1), TreeNode(5))
        ).isValid() shouldEqual false
    }
}

private fun TreeNode.isValid(): Boolean {
    if (left != null && left!!.value > value) return false
    if (right != null && right!!.value <= value) return false
    return (left?.isValid() ?: true) && (right?.isValid() ?: true)
}
