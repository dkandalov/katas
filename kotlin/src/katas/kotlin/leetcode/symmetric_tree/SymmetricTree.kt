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
    }
}

private fun TreeNode.isSymmetric(): Boolean {
    return true
}
