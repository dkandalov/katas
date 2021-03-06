package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.UnweightedGraphs.meshGraph
import katas.kotlin.skiena.graphs.UnweightedGraphs.diamondGraph
import nonstdlib.join
import datsok.shouldEqual
import org.junit.Test

data class Edge<T>(val from: T, val to: T, val weight: Int? = null) {
    override fun toString(): String {
        val weightString = if (weight != null) "/$weight" else ""
        return "$from-$to$weightString"
    }

    companion object {
        fun <T> read(token: String, parse: (String) -> T): Edge<T>? {
            val split = token.split('-', '/')
            val weight = if (split.size == 3) split[2].toInt() else null
            val to = if (split.size >= 2) parse(split[1]) else return null
            val from = parse(split[0])
            return Edge(from, to, weight)
        }
    }
}

data class Graph<T>(val edgesByVertex: MutableMap<T, LinkedHashSet<Edge<T>>> = HashMap()) {
    val vertices: Set<T> get() = edgesByVertex.keys
    val edges: List<Edge<T>> get() = edgesByVertex.values.flatten()

    fun addEdge(fromVertex: T, toVertex: T): Graph<T> = addEdge(Edge(fromVertex, toVertex))

    fun addEdge(edge: Edge<T>): Graph<T> {
        edgesByVertex.getOrPut(edge.from, { LinkedHashSet() }).add(edge)
        edgesByVertex.getOrPut(edge.to, { LinkedHashSet() }).add(Edge(edge.to, edge.from, edge.weight))
        return this
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
                .map { edge ->
                    processedFrom.add(edge.from)
                    processedTo.add(edge.to)
                    edge.toString()
                }
        }.join(",")
    }

    companion object {
        fun read(s: String): Graph<String> = read(s) { it }

        fun readInts(s: String): Graph<Int> = read(s) { it.toInt() }

        fun <T> read(s: String, parse: (String) -> T): Graph<T> {
            val graph = Graph<T>()
            s.split(",").forEach { token ->
                val edge = Edge.read(token, parse)
                if (edge != null) graph.addEdge(edge)
                else graph.addVertex(parse(token))
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
    // 1 -- 2 -- 3
    val linearGraph = Graph.readInts("1-2,2-3")

    // 1 -- 2   3 -- 4
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

object WeightedGraphs {
    // 1 -- 2 -- 3
    val linearGraph = Graph.readInts("1-2/10,2-3/20")

    // 2 -- 3
    //  \  /
    //   1
    val triangleGraph = Graph.readInts("1-2/20,1-3/20,2-3/10")

    //   3
    //  / \
    // 2   4
    //  \ /
    //   1
    val diamondGraph = Graph.readInts("1-2/10,1-4/20,2-3/30,3-4/40")

    // From Skiena Figure 6.3
    val exampleGraph = Graph.read("A-B/5,A-C/7,A-D/12,B-C/9,C-D/4,B-E/7,C-E/4,C-F/3,E-F/2,E-G/5,F-G/2")
}