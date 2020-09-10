package katas.kotlin.leetcode.serialize_and_deserialize_binary_tree

import org.junit.jupiter.api.Test
import java.util.*

// https://leetcode.com/problems/serialize-and-deserialize-binary-tree
//
// Serialization is the process of converting a data structure or object into a sequence of bits
// so that it can be stored in a file or memory buffer, or transmitted across a network connection
// link to be reconstructed later in the same or another computer environment.
//
// Design an algorithm to serialize and deserialize a binary tree. There is no restriction on
// how your serialization/deserialization algorithm should work. You just need to ensure that a binary tree
// can be serialized to a string and this string can be deserialized to the original tree structure.
//
// Example:
// You may serialize the following tree:
//    1
//   / \
//  2   3
//     / \
//    4   5
// as "[1,2,3,null,null,4,5]"
// as "1,2,null,null,3,4,null,null,5,null,null"
//
// Clarification: The above format is the same as how LeetCode serializes a binary tree.
// You do not necessarily need to follow this format, so please be creative and come up with different approaches yourself.
//
// Note: Do not use class member/global/static variables to store states.
// Your serialize and deserialize algorithms should be stateless.

class BinaryTreeSerializationTests {
    @Test fun `some examples`() {
        TODO()
    }
}

fun serialize(root: TreeNode?): String {
    val list = LinkedList<Int?>()
    root?.traversePreOrder { list.add(it) }
    return list.joinToString(prefix = "", separator = ",", postfix = "")
}

fun deserialize(data: String): TreeNode? {
    val tokens = LinkedList(data.split(","))
    if (tokens.first() == "null") return null
    val root = TreeNode(tokens.removeFirst().toInt())
    deserialize(root, tokens)
    return root
}

private fun deserialize(node: TreeNode, tokens: LinkedList<String>) {
    fun String.parseNodeValue(): Int? = if (this == "null") null else toInt()

    node.left = tokens.removeFirst().parseNodeValue()?.let { TreeNode(it) }
    if (node.left != null) deserialize(node.left!!, tokens)

    node.right = tokens.removeFirst().parseNodeValue()?.let { TreeNode(it) }
    if (node.right != null) deserialize(node.right!!, tokens)
}

private fun TreeNode?.traversePreOrder(f: (Int?) -> Unit) {
    if (this == null) return f(null)
    f(`val`)
    left?.traversePreOrder(f)
    right?.traversePreOrder(f)
}

class TreeNode(var `val`: Int) {
    var left: TreeNode? = null
    var right: TreeNode? = null
}