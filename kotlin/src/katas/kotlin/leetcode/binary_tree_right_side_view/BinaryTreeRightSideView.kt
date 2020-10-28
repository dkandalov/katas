package katas.kotlin.leetcode.binary_tree_right_side_view

import datsok.shouldEqual
import org.junit.jupiter.api.Test
import java.util.*
import kotlin.collections.ArrayList

//
// https://leetcode.com/problems/binary-tree-right-side-view ✅
//
// Given a binary tree, imagine yourself standing on the right side of it,
// return the values of the nodes you can see ordered from top to bottom.
//
// Example:
// Input: [1,2,3,null,5,null,4]
// Output: [1, 3, 4]
// Explanation:
//    1            <---
//  /   \
// 2     3         <---
//  \     \
//   5     4       <---
//
// Example:
//    1            <---
//  /   \
// 2     3         <---
//  \    /\
//   5  4  6       <---
//
// Example:
//    1            <---
//  /   \
// 2     3         <---
//  \   /
//   5 4           <---
//

fun rightSideView(root: TreeNode?): List<Int> {
    return if (root == null) emptyList()
    else rightSideViewOf(root).map { it.value }
//    else breadthFirst(root).map { it.last().value }
}

private fun rightSideViewOf(node: TreeNode?): List<TreeNode> {
    if (node == null) return emptyList()
    val leftView = rightSideViewOf(node.left)
    val rightView = rightSideViewOf(node.right)
    return listOf(node) + (rightView + leftView.drop(rightView.size))
}

val TreeNode.value get() = `val`


class Tests {
    @Test fun `some examples`() {
        rightSideView(TreeNode(1)) shouldEqual listOf(1)

        rightSideView(TreeNode(1,
            TreeNode(2, right = TreeNode(5)),
            TreeNode(3, right = TreeNode(4))
        )) shouldEqual listOf(1, 3, 4)

        rightSideView(TreeNode(1,
            TreeNode(2, right = TreeNode(5)),
            TreeNode(3, left = TreeNode(4), right = TreeNode(6))
        )) shouldEqual listOf(1, 3, 6)

        rightSideView(TreeNode(1,
            TreeNode(2, right = TreeNode(5)),
            TreeNode(3, left = TreeNode(4))
        )) shouldEqual listOf(1, 3, 4)

        rightSideView(TreeNode(1,
            TreeNode(2, left = TreeNode(4)),
            TreeNode(3)
        )) shouldEqual listOf(1, 3, 4)
    }
}

private fun breadthFirst(rootNode: TreeNode): List<List<TreeNode>> {
    val queue = LinkedList<List<TreeNode>>()
    queue.add(listOf(rootNode))
    val result = ArrayList<List<TreeNode>>()
    while (queue.isNotEmpty()) {
        val nodes = queue.removeFirst()
        result.add(nodes)

        val elements = ArrayList<TreeNode>().also { list ->
            nodes.forEach {
                if (it.left != null) list.add(it.left!!)
                if (it.right != null) list.add(it.right!!)
            }
        }
        if (elements.isNotEmpty()) queue.add(elements)
    }
    return result
}


data class TreeNode(
    var `val`: Int,
    var left: TreeNode? = null,
    var right: TreeNode? = null,
)

