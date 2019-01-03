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

data class Graph<T>(val edgesByVertex: MutableMap<T, MutableList<Edge<T>>> = HashMap()) {

    val vertices: Set<T> get() = edgesByVertex.keys

    fun addEdge(from: T, to: T) {
        edgesByVertex.getOrPut(from, { ArrayList() }).add(Edge(from, to))
        edgesByVertex.getOrPut(to, { ArrayList() }).add(Edge(to, from))
    }

    fun addVertex(vertex: T) {
        edgesByVertex.getOrPut(vertex, { ArrayList() })
    }

    override fun toString(): String {
        val processedFrom = HashSet<T>()
        val processedTo = HashSet<T>()
        return edgesByVertex.entries.flatMap { (vertex, edges) ->
            if (edges.isEmpty()) listOf("$vertex")
            else edges.filterNot { (from, to) -> processedFrom.contains(to) && processedTo.contains(from) }
                .map { (from, to) ->
                    processedFrom.add(from)
                    processedTo.add(to)
                    "$from-$to"
                }
        }.join(",")
    }

    companion object {
        fun read(s: String): Graph<String> = read(s) { it }

        fun readInts(s: String): Graph<Int> = read(s) { it.toInt() }

        fun <T> read(s: String, parse: (String) -> T): Graph<T> {
            val graph = Graph<T>()
            s.split(",").forEach { token ->
                val split = token.split("-")
                if (split.size == 2) {
                    graph.addEdge(from = parse(split[0]), to = parse(split[1]))
                } else {
                    graph.addVertex(parse(split[0]))
                }
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
        Graph.readInts("1-2,3").toString() shouldEqual "1-2,3"

        diamondGraph.toString() shouldEqual "1-2,1-4,2-3,3-4"
        meshGraph.toString() shouldEqual "1-2,1-3,1-4,2-3,2-4,3-4"
    }

    companion object {
        val linearGraph = Graph.readInts("1-2,2-3")

        val disconnectedGraph = Graph.readInts("1-2,3-4")

        //   3
        //  / \
        // 2   4
        //  \ /
        //   1
        val diamondGraph = Graph.readInts("1-2,1-4,2-3,3-4")

        //   3
        //  /|\
        // 2-+-4
        //  \|/
        //   1
        val meshGraph = Graph.readInts("1-2,1-3,1-4,2-3,2-4,3-4")
    }
}
