package katas.kotlin.leetcode.array_to_bst

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

class ArrayToBSTTests {
    @Test fun `convert sorted array to balanced BST`() {
        arrayOf(1).toBST_() shouldEqual TreeNode(1)
        arrayOf(1, 2).toBST_() shouldEqual TreeNode(2, TreeNode(1))
        arrayOf(1, 2, 3).toBST_() shouldEqual TreeNode(2, TreeNode(1), TreeNode(3))
        arrayOf(1, 2, 3, 4).toBST_() shouldEqual TreeNode(3, TreeNode(2, TreeNode(1)), TreeNode(4))
        arrayOf(1, 2, 3, 4, 5).toBST_() shouldEqual
            TreeNode(3,
                TreeNode(2, TreeNode(1)),
                TreeNode(4, null, TreeNode(5))
            )
    }
}

private fun Array<Int>.toBST_(): TreeNode {
    val midIndex = size / 2
    val node = TreeNode(this[midIndex])
    (midIndex - 1 downTo 0).forEach { i -> node.add(this[i]) }
    (midIndex + 1 until size).forEach { i -> node.add(this[i]) }
    return node
}

private fun TreeNode.add(value: Int) {
    if (value <= this.value) {
        if (left == null) left = TreeNode(value)
        else left!!.add(value)
    } else {
        if (right == null) right = TreeNode(value)
        else right!!.add(value)
    }
}