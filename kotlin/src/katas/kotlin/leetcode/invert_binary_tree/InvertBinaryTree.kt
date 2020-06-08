@file:Suppress("unused")

package katas.kotlin.leetcode.invert_binary_tree

import katas.kotlin.leetcode.TreeNode
import datsok.shouldEqual
import org.junit.Test

class InvertBinaryTreeTests {
    @Test fun `invert a binary tree`() {
        TreeNode(1).invert() shouldEqual TreeNode(1)
        TreeNode(2, TreeNode(1), TreeNode(3)).invert() shouldEqual TreeNode(2, TreeNode(3), TreeNode(1))

        TreeNode(4,
            TreeNode(2, TreeNode(1), TreeNode(3)),
            TreeNode(7, TreeNode(6), TreeNode(9))
        ).invert() shouldEqual
            TreeNode(4,
                TreeNode(7, TreeNode(9), TreeNode(6)),
                TreeNode(2, TreeNode(3), TreeNode(1))
            )
    }
}

private fun TreeNode.invert(): TreeNode {
    return TreeNode(
        value = value,
        left = right?.invert(),
        right = left?.invert()
    )
}

private fun TreeNode.invert_mutating(): TreeNode {
    val tmp = left
    left = right
    right = tmp

    left?.invert()
    right?.invert()

    return this
}
