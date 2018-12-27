package katas.kotlin.skiena.graph2

import kotlincommon.join
import kotlincommon.test.shouldEqual
import org.junit.Test

data class Edge<T>(var from: T, var to: T, var weight: Int? = null) {
    override fun toString(): String {
        val weightString = if (weight != null) ", $weight" else ""
        return "Edge($from->$to$weightString)"
    }
}

class Graph<T>(val edgesByVertex: MutableMap<T, MutableList<Edge<T>>> = HashMap()) {

    val vertices: Set<T> get() = edgesByVertex.keys

    fun insertEdge(from: T, to: T) {
        edgesByVertex.getOrPut(from, { ArrayList() }).add(Edge(from, to))
        edgesByVertex.getOrPut(to, { ArrayList() }).add(Edge(to, from))
    }

    override fun toString(): String {
        val processedFrom = HashSet<T>()
        val processedTo = HashSet<T>()
        return edgesByVertex.keys.flatMap { vertex ->
            val edges = edgesByVertex[vertex] ?: emptyList<Edge<T>>()
            edges
                .filterNot { edge ->
                    processedFrom.contains(edge.to) && processedTo.contains(vertex)
                }
                .map { edge ->
                    processedFrom.add(vertex)
                    processedTo.add(edge.to)
                    "$vertex-${edge.to}"
                }
        }.join(",")
    }

    companion object {
        fun read(s: String): Graph<String> = read(s) { it }

        fun readInts(s: String): Graph<Int> = read(s) { it.toInt() }

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
        Graph.readInts("1-2").toString() shouldEqual "1-2"
        Graph.readInts("2-1").toString() shouldEqual "1-2"

        Graph.readInts("1-2,2-3").toString() shouldEqual "1-2,2-3"

        // 1──2──4
        // └──3──┘
        graphWithCycle.toString() shouldEqual "1-2,1-3,2-4,3-4"
    }

    companion object {
        // 1──2──4
        // └──3──┘
        val graphWithCycle = Graph.readInts("1-2,1-3,2-4,3-4")
    }
}
