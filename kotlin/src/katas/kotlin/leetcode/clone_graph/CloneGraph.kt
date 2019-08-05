package katas.kotlin.leetcode.clone_graph

import kotlincommon.test.shouldEqual
import org.junit.Test

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

class GraphNodeTests {
    @Test fun `convert to map`() {
        GraphNode(1).toMap() shouldEqual mapOf(1 to emptySet())

        UndirectedGraph().connect(1, 2).nodes.first().toMap() shouldEqual mapOf(
            1 to setOf(2),
            2 to setOf(1)
        )
        UndirectedGraph().connect(1, 2).connect(2, 3).nodes.first().toMap() shouldEqual mapOf(
            1 to setOf(2),
            2 to setOf(1, 3),
            3 to setOf(2)
        )
        UndirectedGraph().connect(1, 2).connect(2, 3).connect(3, 1).nodes.first().toMap() shouldEqual mapOf(
            1 to setOf(2, 3),
            2 to setOf(1, 3),
            3 to setOf(1, 2)
        )
    }
    
    @Test fun `create graph from string`() {
        UndirectedGraph.parse("1-2")
    }
}

private fun GraphNode.toMap(result: HashMap<Int, Set<Int>> = HashMap()): Map<Int, Set<Int>> {
    if (result.containsKey(value)) return result
    result[value] = neighbors.map { it.value }.toSet()
    neighbors.forEach { it.toMap(result) }
    return result
}

data class GraphNode(var value: Int, var neighbors: MutableList<GraphNode> = ArrayList())

data class UndirectedGraph(val nodes: MutableSet<GraphNode> = LinkedHashSet()) {
    fun connect(value1: Int, value2: Int): UndirectedGraph {
        val node1 = nodes.find { it.value == value1 } ?: GraphNode(value1).also { nodes.add(it) }
        val node2 = nodes.find { it.value == value2 } ?: GraphNode(value2).also { nodes.add(it) }
        node1.neighbors.add(node2)
        node2.neighbors.add(node1)
        return this
    }

    companion object {
        fun parse(s: String): UndirectedGraph {
            val graph = UndirectedGraph()
            s.split(",").forEach { token ->
                val list = token.split("-")
                graph.connect(list[0].toInt(), list[1].toInt())
            }
            return graph
        }
    }
}
