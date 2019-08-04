package katas.kotlin.leetcode.array_to_bst

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

class ArrayToBSTTests {
    @Test fun `convert array to balanced BST`() {
        arrayOf(1).toBST() shouldEqual TreeNode(1)
    }
}

private fun Array<Int>.toBST(): TreeNode {
    return TreeNode(first())
}
