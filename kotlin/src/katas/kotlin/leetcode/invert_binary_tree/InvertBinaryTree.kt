package katas.kotlin.leetcode.invert_binary_tree

import kotlincommon.test.shouldEqual
import org.junit.Test

class InvertBinaryTreeTests {
    @Test fun `invert a binary tree`() {
        TreeNode(1).invert() shouldEqual TreeNode(1)
        TreeNode(2, TreeNode(1), TreeNode(3)).invert() shouldEqual TreeNode(2, TreeNode(3), TreeNode(1))
    }
}

private fun TreeNode.invert(): TreeNode {
    val tmp = left?.value
    right?.value?.let { left?.value = it }
    tmp?.let { right?.value = it }

    return this
}

data class TreeNode(
    var value: Int,
    var left: TreeNode? = null,
    var right: TreeNode? = null
)