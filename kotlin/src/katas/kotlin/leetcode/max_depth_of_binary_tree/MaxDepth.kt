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
    }
}

private fun TreeNode?.maxDepth(depth: Int = 0): Int {
    if (this == null) return depth
    return left.maxDepth(depth + 1)
}
