package graph

import kotlin.test.Test
import kotlin.test.assertEquals

data class EdgeNode(
    var y: Int,
    var weight: Int?,
    var next: EdgeNode?
)

class Graph(
    var edges: Array<EdgeNode?>,
    var degree: Array<Int>,
    var numberOfVertices: Int = 0,
    var numberOfEdges: Int = 0,
    var directed: Boolean
) {
    constructor(size: Int, directed: Boolean) : this(
        edges = Array(size, { _ -> null }),
        degree = Array(size, { _ -> 0 }),
        directed = directed
    )
}

fun readGraph(s: String): Graph {
    val graph = Graph(size = 100, directed = false)
    s.split(",").forEach {
        val (x, y) = it.split("-").let { Pair(it[0].toInt(), it[1].toInt()) }
        val next = if (x < graph.edges.size) graph.edges[x] else null
        val edgeNode = EdgeNode(y = y, weight = null, next = next)
        graph.edges[x] = edgeNode
        graph.degree[x]++
    }
    return graph
}

class GraphTest {
    @Test fun `create graph from string`() {
        readGraph("1-2").let {
            assertEquals("", it.toString())
        }
    }
}