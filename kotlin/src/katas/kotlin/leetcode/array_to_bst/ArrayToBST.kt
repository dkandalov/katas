package katas.kotlin.leetcode.array_to_bst

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test
import javax.xml.soap.Node

class ArrayToBSTTests {
    @Test fun `convert sorted array to balanced BST`() {
        arrayOf(1).toBST() shouldEqual TreeNode(1)
        arrayOf(1, 2).toBST() shouldEqual TreeNode(2, TreeNode(1))
        arrayOf(1, 2, 3).toBST() shouldEqual TreeNode(2, TreeNode(1), TreeNode(3))
    }
}

private fun Array<Int>.toBST(): TreeNode {
    val midIndex = size / 2
    val node = TreeNode(this[midIndex])
    (0 until midIndex).forEach { i ->
        node.left = TreeNode(this[i])
    }
    (midIndex + 1 until size).forEach { i ->
        node.right = TreeNode(this[i])
    }
    return node
}
