package katas.kotlin.leetcode.invert_binary_tree

import kotlincommon.test.shouldEqual
import org.junit.Test

class InvertBinaryTreeTests {
    @Test fun `invert a binary tree`() {
        TreeNode(1).invert() shouldEqual TreeNode(1)
    }
}

private fun TreeNode.invert(): TreeNode {
    return this
}

data class TreeNode(var value: Int) {
    var left: TreeNode? = null
    var right: TreeNode? = null
}