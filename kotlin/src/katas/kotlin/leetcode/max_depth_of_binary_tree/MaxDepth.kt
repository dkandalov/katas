package katas.kotlin.leetcode.max_depth_of_binary_tree

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

class MaxDepthTests {
    @Test fun `find max depth of a binary tree`() {
        null.maxDepth() shouldEqual 0
        TreeNode(1).maxDepth() shouldEqual 1
        TreeNode(1, TreeNode(0)).maxDepth() shouldEqual 2
        TreeNode(1, TreeNode(0), TreeNode(2)).maxDepth() shouldEqual 2
        TreeNode(1,
            TreeNode(0, TreeNode(-1)),
            TreeNode(2)
        ).maxDepth() shouldEqual 3
        TreeNode(1,
            TreeNode(0),
            TreeNode(2, TreeNode(3))
        ).maxDepth() shouldEqual 3
    }
}

private fun TreeNode?.maxDepth(): Int {
    if (this == null) return 0
    return maxOf(left.maxDepth() + 1, right.maxDepth() + 1)
}

private fun TreeNode?.maxDepth_(): Int {
    if (this == null) return 0
    return maxOf(left.maxDepth() + 1, right.maxDepth() + 1)
}
