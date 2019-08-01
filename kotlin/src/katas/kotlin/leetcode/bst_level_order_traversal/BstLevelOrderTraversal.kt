package katas.kotlin.leetcode.bst_level_order_traversal

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.ArrayList

class BstLevelOrderTraversal {
    @Test fun `level order traversal`() {
        TreeNode(1).levelOrder() shouldEqual listOf(listOf(1))
        TreeNode(1, TreeNode(0), TreeNode(2)).levelOrder() shouldEqual listOf(listOf(1), listOf(0, 2))
    }
}

private fun TreeNode.levelOrder(): List<List<Int>> {
    var q1 = LinkedList<TreeNode>()
    var q2 = LinkedList<TreeNode>()
    q1.addFirst(this)
    val result = ArrayList<List<Int>>()
    while (q1.isNotEmpty()) {
        val subResult = ArrayList<Int>()
        while (q1.isNotEmpty()) {
            val node = q1.removeLast()
            if (node.left != null) q2.addFirst(node.left)
            if (node.right != null) q2.addFirst(node.right)
            subResult.add(node.value)
        }
        result.add(subResult)
        val tmp = q1
        q1 = q2
        q2 = tmp
    }
    return result
}
