package katas.kotlin.leetcode.bst_level_order_traversal

import katas.kotlin.leetcode.TreeNode
import datsok.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.ArrayList

/**
 * https://leetcode.com/problems/binary-tree-level-order-traversal
 */
class BstLevelOrderTraversal2 {
    @Test fun `it works`() {
        levelOrder(TreeNode(1)) shouldEqual listOf(listOf(1))
        levelOrder(
            TreeNode(1,
                TreeNode(2),
                TreeNode(3)
            )
        ) shouldEqual listOf(listOf(1), listOf(2, 3))

        levelOrder(
            TreeNode(3,
                TreeNode(9, TreeNode(6)),
                TreeNode(20, TreeNode(15), TreeNode(7))
            )
        ) shouldEqual listOf(listOf(3), listOf(9, 20), listOf(6, 15, 7))
    }

    private fun levelOrder(
        node: TreeNode?,
        depth: Int = 0,
        result: ArrayList<ArrayList<Int>> = ArrayList()
    ): List<List<Int>> {
        if (node == null) return result
        if (result.size <= depth) result.add(ArrayList())
        result[depth].add(node.value)

        levelOrder(node.left, depth + 1, result)
        levelOrder(node.right, depth + 1, result)

        return result
    }

    private fun levelOrder_(node: TreeNode?): List<List<Int>> {
        if (node == null) return emptyList()

        val queue = LinkedList<List<TreeNode>>()
        queue.addFirst(listOf(node))
        val result = ArrayList<List<Int>>()

        while (queue.isNotEmpty()) {
            val nodesAtLevel = queue.removeLast()
            result.add(nodesAtLevel.map { it.value })

            val nodesAtNextLevel = nodesAtLevel.flatMap {
                listOfNotNull(it.left, it.right)
            }
            if (nodesAtNextLevel.isNotEmpty()) {
                queue.add(nodesAtNextLevel)
            }
        }
        return result
    }
}
