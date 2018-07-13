package graph

import kotlin.test.Test
import kotlin.test.assertEquals

data class EdgeNode(
    var y: Int,
    var weight: Int?,
    var next: EdgeNode?
)

class Graph(
    var edges: ArrayList<EdgeNode?>,
    var degree: ArrayList<Int>,
    var numberOfVertices: Int = 0,
    var numberOfEdges: Int = 0,
    var directed: Boolean
) {
    constructor(size: Int, directed: Boolean) : this(
        edges = ArrayList<EdgeNode?>().fillWith(null, size),
        degree = ArrayList<Int>().fillWith(0, size),
        directed = directed
    )

    fun insertEdge(x: Int, y: Int) {
        val nextEdgeNode = if (x < edges.size) edges[x] else null
        val edgeNode = EdgeNode(y = y, weight = null, next = nextEdgeNode)
        edges[x] = edgeNode
        degree[x]++
    }

    override fun toString(): String {
        return 0.until(numberOfVertices).joinToString("\n") { i ->
            "$i: " + edges.fold("") { acc, it ->
                if (it == null) acc else acc + " " + it.y
            }
        }
    }

    companion object {
        fun read(s: String): Graph {
            val graph = Graph(size = 100, directed = false)
            val vertices = HashMap<Int, Unit>()
            s.split(",").forEach {
                val (x, y) = it.split("-").let { Pair(it[0].toInt(), it[1].toInt()) }
                graph.insertEdge(x, y)
                vertices[x] = Unit
                vertices[y] = Unit
            }
            graph.numberOfVertices = vertices.size
            return graph
        }
    }
}

class GraphTest {
    @Test fun `create graph from string`() {
        Graph.read("1-2").let {
            assertEquals("1-2", it.toString())
        }
    }
}

fun <T> ArrayList<T>.fillWith(value: T, size: Int): ArrayList<T> {
    0.until(size).forEach { add(value) }
    return this
}