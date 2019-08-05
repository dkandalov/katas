package katas.kotlin.leetcode.clone_graph

import kotlincommon.test.shouldEqual
import org.junit.Test

data class GraphNode(var value: Int, var neighbors: MutableList<GraphNode> = ArrayList())

class CloneGraphTests {
    @Test fun `clone single node`() {
        GraphNode(1).cloneGraph() shouldEqual GraphNode(1)
    }
}

private fun GraphNode.cloneGraph(): GraphNode {
    return this
}
