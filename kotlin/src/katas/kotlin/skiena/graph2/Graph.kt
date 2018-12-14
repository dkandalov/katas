package katas.kotlin.skiena.graph2

import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test

data class Edge<T>(
    var toVertex: T,
    var next: Edge<T>?,
    var weight: Int? = null
) : Iterable<Edge<T>> {
    override fun iterator(): Iterator<Edge<T>> =
        generateSequence(this) { edgeNode -> edgeNode.next }.iterator()
}

class Graph<T>(val edgesByVertex: MutableMap<T, Edge<T>?> = HashMap()) {

    val vertices: Set<T> get() = edgesByVertex.keys

    fun insertEdge(from: T, to: T) {
        edgesByVertex[from] = Edge(toVertex = to, next = edgesByVertex[from])
        edgesByVertex[to] = Edge(toVertex = from, next = edgesByVertex[to])
    }

    override fun toString(): String {
        val processedFrom = HashSet<T>()
        val processedTo = HashSet<T>()
        return edgesByVertex.keys.flatMap { vertex ->
            val edges = edgesByVertex[vertex] ?: emptyList<Edge<T>>()
            edges
                .filterNot { edge ->
                    processedFrom.contains(edge.toVertex) && processedTo.contains(vertex)
                }
                .map { edge ->
                    processedFrom.add(vertex)
                    processedTo.add(edge.toVertex)
                    "$vertex-${edge.toVertex}"
                }
        }.join(",")
    }

    companion object {
        fun read(s: String): Graph<String> = read(s) { it }

        fun readInt(s: String): Graph<Int> = read(s) { it.toInt() }

        fun <T> read(s: String, parse: (String) -> T): Graph<T> {
            val graph = Graph<T>()
            s.split(",").forEach { token ->
                val (x, y) = token.split("-").map { parse(it) }
                graph.insertEdge(x, y)
            }
            return graph
        }
    }
}

class GraphTest {
    @Test fun `create undirected graph from string`() {
        Graph.readInt("1-2").toString() shouldEqual "1-2"
        Graph.readInt("2-1").toString() shouldEqual "1-2"

        Graph.readInt("1-2,2-3").toString() shouldEqual "1-2,2-3"

        // 1──2──4
        // └──3──┘
        graphWithCycle.toString() shouldEqual "1-3,1-2,2-4,3-4"
    }

    companion object {
        // 1──2──4
        // └──3──┘
        val graphWithCycle = Graph.readInt("1-2,1-3,2-4,3-4")
    }
}
