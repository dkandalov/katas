package katas.kotlin.leetcode.vertical_traversla_of_bst

import org.junit.Test

class VerticalTraversalTests {
    @Test fun `vertical order traversal`() {
        require(verticalTraversal(Node(1)) == listOf(listOf(1)))

        val tree = Node(3,
            Node(9),
            Node(20, Node(15), Node(7))
        )
        require(
            verticalTraversal(tree) == listOf(
                listOf(9), listOf(3, 15), listOf(20), listOf(7))
        )

        val tree2 = Node(1,
            Node(2, Node(4), Node(5)),
            Node(3, Node(6), Node(7))
        )
        require(
            verticalTraversal(tree2) == listOf(
                listOf(4), listOf(2), listOf(1, 5, 6), listOf(3), listOf(7))
        )
    }
}

private fun verticalTraversal(root: Node): List<List<Int>> {
    val valuesByColumn = java.util.TreeMap<Int, ArrayList<Int>>()
    root.traverse { node, column ->
        val values = valuesByColumn.getOrPut(column, { ArrayList() })
        values.add(node.value)
    }
    return valuesByColumn.values.map { it }
}

private fun Node.traverse(column: Int = 0, f: (Node, Int) -> Unit) {
    f(this, column)
    left?.traverse(column - 1, f)
    right?.traverse(column + 1, f)
}

private data class Node(
    val value: Int,
    val left: Node? = null,
    val right: Node? = null
)
