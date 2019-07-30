package katas.kotlin.leetcode.invert_binary_tree

import org.junit.Test

class InvertBinaryTreeTests {
    @Test fun `invert a binary tree`() {
        TreeNode(1).invert()
    }
}

private fun TreeNode.invert() {
}

class TreeNode(var value: Int) {
    var left: TreeNode? = null
    var right: TreeNode? = null
}