package katas.kotlin.leetcode.clone_graph

import kotlincommon.test.shouldEqual
import org.junit.Test

data class GraphNode(var value: Int, var neighbors: MutableList<GraphNode> = ArrayList())

data class UndirectedGraph(val nodes: MutableSet<GraphNode> = LinkedHashSet()) {
    fun connect(value1: Int, value2: Int): UndirectedGraph {
        val node1 = nodes.find { it.value == value1 } ?: GraphNode(value1).also { nodes.add(it) }
        val node2 = nodes.find { it.value == value2 } ?: GraphNode(value2).also { nodes.add(it) }
        node1.neighbors.add(node2)
        node2.neighbors.add(node1)
        return this
    }
}

class CloneGraphTests {
    @Test fun `clone single node`() {
        GraphNode(1).cloneGraph() shouldEqual GraphNode(1)
    }

    @Test fun `clone two nodes`() {
        val graph = UndirectedGraph().connect(1, 2)
        graph.nodes.first().cloneGraph() shouldEqual graph.nodes.first()
    }
}

private fun GraphNode.cloneGraph(): GraphNode {
    return this
}
