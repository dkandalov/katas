package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.UnweightedGraphs.meshGraph
import katas.kotlin.skiena.graphs.UnweightedGraphs.diamondGraph
import kotlincommon.join
import kotlincommon.test.shouldEqual
import org.junit.Test

data class Edge<T>(val from: T, val to: T, val weight: Int? = null) {
    override fun toString(): String {
        val weightString = if (weight != null) ", $weight" else ""
        return "Edge($from->$to$weightString)"
    }
}

data class Graph<T>(val edgesByVertex: MutableMap<T, LinkedHashSet<Edge<T>>> = HashMap()) {

    val vertices: Set<T> get() = edgesByVertex.keys

    fun addEdge(from: T, to: T, weight: Int? = null) {
        edgesByVertex.getOrPut(from, { LinkedHashSet() }).add(Edge(from, to, weight))
        edgesByVertex.getOrPut(to, { LinkedHashSet() }).add(Edge(to, from, weight))
    }

    fun addVertex(vertex: T) {
        edgesByVertex.getOrPut(vertex, { LinkedHashSet() })
    }

    override fun toString(): String {
        val processedFrom = HashSet<T>()
        val processedTo = HashSet<T>()
        return edgesByVertex.entries.flatMap { (vertex, edges) ->
            if (edges.isEmpty()) listOf("$vertex")
            else edges.filterNot { (from, to) -> processedFrom.contains(to) && processedTo.contains(from) }
                .map { (from, to, weight) ->
                    processedFrom.add(from)
                    processedTo.add(to)
                    val weightString = if (weight != null) "/$weight" else ""
                    "$from-$to$weightString"
                }
        }.join(",")
    }

    companion object {
        fun read(s: String): Graph<String> = read(s) { it }

        fun readInts(s: String): Graph<Int> = read(s) { it.toInt() }

        fun <T> read(s: String, parse: (String) -> T): Graph<T> {
            val graph = Graph<T>()
            s.split(",").forEach { token ->
                val split = token.split('-', '/')
                val weight = if (split.size == 3) split[2].toInt() else null
                val to = if (split.size >= 2) parse(split[1]) else null
                val from = parse(split[0])

                if (to != null) graph.addEdge(from, to, weight)
                else graph.addVertex(from)
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

    @Test fun `create undirected weighted graph from string`() {
        Graph.readInts("1-2/10").toString() shouldEqual "1-2/10"
        Graph.readInts("2-1/10").toString() shouldEqual "1-2/10"
        Graph.readInts("1-2/10,2-3/20").toString() shouldEqual "1-2/10,2-3/20"
        Graph.readInts("1-2/10,3").toString() shouldEqual "1-2/10,3"
    }
}

object UnweightedGraphs {
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
