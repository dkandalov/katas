package katas.kotlin.leetcode.symmetric_tree

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*

/**
 * https://leetcode.com/problems/symmetric-tree/
 */
class SymmetricTreeTests {
    @Test fun `check is tree is a mirror of itself`() {
        TreeNode(1).isSymmetric() shouldEqual true
        TreeNode(1, TreeNode(0), TreeNode(0)).isSymmetric() shouldEqual true
        TreeNode(1, TreeNode(0), TreeNode(2)).isSymmetric() shouldEqual false
        TreeNode(1,
            TreeNode(2, TreeNode(3)),
            TreeNode(2, null, TreeNode(3))
        ).isSymmetric() shouldEqual true
        TreeNode(1,
            TreeNode(2, TreeNode(3)),
            TreeNode(2, TreeNode(3))
        ).isSymmetric() shouldEqual false
    }
}

private fun TreeNode.isSymmetric(): Boolean {
    val leftQueue = LinkedList<TreeNode?>()
    val rightQueue = LinkedList<TreeNode?>()
    leftQueue.add(left)
    rightQueue.add(right)
    while (leftQueue.isNotEmpty() && rightQueue.isNotEmpty()) {
        val l = leftQueue.removeFirst()
        val r = rightQueue.removeFirst()
        if (l?.value != r?.value) return false
        if (l != null) {
            leftQueue.add(l.left)
            leftQueue.add(l.right)
        }
        if (r != null) {
            rightQueue.add(r.right)
            rightQueue.add(r.left)
        }
    }
    return leftQueue.isEmpty() && rightQueue.isEmpty()
}

private fun TreeNode.isSymmetric_() = inverted() == this

private fun TreeNode.inverted(): TreeNode =
    TreeNode(value, left = right?.inverted(), right = left?.inverted())
