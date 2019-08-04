package katas.kotlin.leetcode.symmetric_tree

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/symmetric-tree/
 */
class SymmetricTreeTests {
    @Test fun `check is tree is a mirror of itself`() {
        TreeNode(1).isSymmetric() shouldEqual true
        TreeNode(1, TreeNode(0), TreeNode(0)).isSymmetric() shouldEqual true
        TreeNode(1, TreeNode(0), TreeNode(2)).isSymmetric() shouldEqual false
        TreeNode(1,
            TreeNode(2, TreeNode(3)),
            TreeNode(2, null, TreeNode(3))
        ).isSymmetric() shouldEqual true
        TreeNode(1,
            TreeNode(2, TreeNode(3)),
            TreeNode(2, TreeNode(3))
        ).isSymmetric() shouldEqual false
    }
}

private fun TreeNode.isSymmetric() = inverted() == this

private fun TreeNode.inverted(): TreeNode =
    TreeNode(value, left = right?.inverted(), right = left?.inverted())
