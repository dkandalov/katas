package katas.kotlin.leetcode.bst_level_order_traversal

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

class BstLevelOrderTraversal {
    @Test fun `level order traversal`() {
        TreeNode(1).levelOrder() shouldEqual listOf(listOf(1))
    }
}

private fun TreeNode.levelOrder(): List<List<Int>> {
    return listOf(listOf(value))
}
