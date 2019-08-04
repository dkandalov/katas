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

    @Test fun `convert sorted array to filled balanced BST`() {
        arrayOf(1).toBST() shouldEqual TreeNode(1)
        arrayOf(1, 2).toBST() shouldEqual TreeNode(2, TreeNode(1))
        arrayOf(1, 2, 3).toBST() shouldEqual TreeNode(2, TreeNode(1), TreeNode(3))
        arrayOf(1, 2, 3, 4).toBST() shouldEqual TreeNode(3, TreeNode(2, TreeNode(1)), TreeNode(4))
        arrayOf(1, 2, 3, 4, 5).toBST() shouldEqual
            TreeNode(3,
                TreeNode(2, TreeNode(1)),
                TreeNode(5, TreeNode(4))
            )
        arrayOf(1, 2, 3, 4, 5, 6).toBST() shouldEqual
            TreeNode(4,
                TreeNode(2, TreeNode(1), TreeNode(3)),
                TreeNode(6, TreeNode(5))
            )
        arrayOf(1, 2, 3, 4, 5, 6, 7).toBST() shouldEqual
            TreeNode(4,
                TreeNode(2, TreeNode(1), TreeNode(3)),
                TreeNode(6, TreeNode(5), TreeNode(7))
            )
    }
}

private fun Array<Int>.toBST(from: Int = 0, to: Int = size): TreeNode? {
    if (to <= from) return null
    val mid = (from + to) / 2
    val node = TreeNode(this[mid])
    node.left = toBST(from, mid)
    node.right = toBST(mid + 1, to)
    return node
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
