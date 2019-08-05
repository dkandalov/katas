package katas.kotlin.leetcode.clone_graph

import kotlincommon.test.shouldEqual
import org.junit.Test

data class GraphNode(var value: Int, var neighbors: MutableList<GraphNode> = ArrayList())

class CloneGraphTests {
    @Test fun `clone single node`() {
        GraphNode(1).cloneGraph() shouldEqual GraphNode(1)
    }

    @Test fun `clone two nodes`() {
        val node1 = GraphNode(1)
        val node2 = GraphNode(2)
        node1.neighbors.add(node2)
        node2.neighbors.add(node1)

        node1.cloneGraph() shouldEqual node1
    }
}

private fun GraphNode.cloneGraph(): GraphNode {
    return this
}
