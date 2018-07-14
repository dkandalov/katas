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
    constructor(directed: Boolean) : this(
        edges = ArrayList<EdgeNode?>(),
        degree = ArrayList<Int>(),
        directed = directed
    )

    fun insertEdge(x: Int, y: Int) {
        edges.ensureSize(x + 1, defaultValue = null)
        degree.ensureSize(x + 1, defaultValue = 0)

        edges[x] = EdgeNode(y = y, weight = null, next = edges[x])
        degree[x]++
    }

    override fun toString(): String {
        return edges.indices.map { x ->
            var s = ""
            var edge = edges[x]
            while (edge != null) {
                s += x.toString() + "-" + edge.y
                edge = edge.next
                if (edge != null) s += ", "
            }
            s
        }.filter { it.isNotEmpty() }.joinToString(",")
    }

    companion object {
        fun read(s: String): Graph {
            val graph = Graph(directed = false)
            val vertices = HashMap<Int, Unit>() // use map because clion can't process HashSet at the moment :(
            s.split(",").forEach {
                val (x, y) = it.split("-").let { Pair(it[0].toInt(), it[1].toInt()) }
                graph.insertEdge(x, y)
                vertices[x] = Unit
                vertices[y] = Unit
                graph.numberOfVertices = vertices.size
            }
            return graph
        }
    }
}

class GraphTest {
    @Test fun `create graph from string`() {
        Graph.read("1-2").toString() shouldEqual "1-2"
        Graph.read("2-1").toString() shouldEqual "2-1"
    }
}

fun <T> ArrayList<T>.fillWith(value: T, size: Int): ArrayList<T> {
    0.until(size).forEach { add(value) }
    return this
}

fun <T> ArrayList<T>.ensureSize(minSize: Int, defaultValue: T): ArrayList<T> {
    if (size < minSize) {
        0.until(minSize - size).forEach { add(defaultValue) }
    }
    return this
}

infix fun <T> T.shouldEqual(that: T) {
    assertEquals(that, this)
}
