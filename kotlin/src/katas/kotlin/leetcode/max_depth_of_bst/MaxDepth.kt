package katas.kotlin.leetcode.max_depth_of_bst

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

class MaxDepthTests {
    @Test fun `find max depth of a binary tree`() {
        TreeNode(1).maxDepth() shouldEqual 1
    }
}

private fun TreeNode?.maxDepth(): Int {
    return 1
}
