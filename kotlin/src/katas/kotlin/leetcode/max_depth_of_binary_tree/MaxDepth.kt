@file:Suppress("unused")

package katas.kotlin.leetcode.max_depth_of_binary_tree

import katas.kotlin.leetcode.TreeNode
import datsok.shouldEqual
import org.junit.Test
import java.util.*

/**
 * https://leetcode.com/problems/maximum-depth-of-binary-tree/
 */
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

private val markNode = TreeNode(Int.MAX_VALUE)

private fun TreeNode?.maxDepth(): Int {
    if (this == null) return 0
    var depth = 0
    val queue = LinkedList<TreeNode>()
    queue.add(this)
    queue.add(markNode)
    while (queue.isNotEmpty()) {
        val node = queue.removeFirst()
        if (node === markNode) {
            depth++
            if (queue.isNotEmpty()) queue.add(markNode)
        } else {
            if (node.left != null) queue.add(node.left!!)
            if (node.right != null) queue.add(node.right!!)
        }
    }
    return depth
}

private fun TreeNode?.maxDepth_(): Int {
    if (this == null) return 0
    return maxOf(left.maxDepth_() + 1, right.maxDepth_() + 1)
}
