package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.DirectedGraphs.diamondGraph
import katas.kotlin.skiena.graphs.DirectedGraphs.meshGraph
import kotlincommon.join
import kotlincommon.test.shouldEqual
import org.junit.Test

// https://cs.stackexchange.com/questions/55041/residual-graph-in-maximum-flow
class NetworkFlows

interface EdgeType<T> {
    val from: T
    val to: T
}

data class DirectedEdge<T>(override val from: T, override val to: T): EdgeType<T> {
    override fun toString() = "$from-$to"

    companion object {
        fun <T> read(token: String, parse: (String) -> T): DirectedEdge<T>? {
            val split = token.split('-')
            return DirectedEdge(
                from = parse(split[0]),
                to = if (split.size >= 2) parse(split[1]) else return null
            )
        }
    }
}

data class DirectedGraph<T, E: EdgeType<T>>(val edgesByVertex: MutableMap<T, LinkedHashSet<E>> = HashMap()) {
    val vertices: Set<T> get() = edgesByVertex.keys
    val edges: List<E> get() = edgesByVertex.values.flatten()

    fun addEdge(edge: E) {
        edgesByVertex.getOrPut(edge.from, { LinkedHashSet() }).add(edge)
    }

    fun addVertex(vertex: T) {
        edgesByVertex.getOrPut(vertex, { LinkedHashSet() })
    }

    override fun toString(): String {
        return edgesByVertex.entries.flatMap { (vertex, edges) ->
            if (edges.isEmpty()) listOf("$vertex")
            else edges.map { it.toString() }
        }.join(",")
    }

    companion object {
        fun read(s: String): DirectedGraph<String, DirectedEdge<String>> = read(s) { it }

        fun readInts(s: String): DirectedGraph<Int, DirectedEdge<Int>> = read(s) { it.toInt() }

        fun <T> read(s: String, parse: (String) -> T): DirectedGraph<T, DirectedEdge<T>> {
            val graph = DirectedGraph<T, DirectedEdge<T>>()
            s.split(",").forEach { token ->
                val edge = DirectedEdge.read(token, parse)
                if (edge != null) graph.addEdge(edge) else graph.addVertex(parse(token))
            }
            return graph
        }
    }
}

class DirectedGraphTest {
    @Test fun `create undirected graph from string`() {
        DirectedGraph.readInts("1-2").toString() shouldEqual "1-2"
        DirectedGraph.readInts("2-1").toString() shouldEqual "2-1"
        DirectedGraph.readInts("1-2,2-3").toString() shouldEqual "1-2,2-3"
        DirectedGraph.readInts("1-2,3").toString() shouldEqual "1-2,3"

        diamondGraph.toString() shouldEqual "1-2,1-4,2-3,3-4"
        meshGraph.toString() shouldEqual "1-2,1-3,1-4,2-3,2-4,3-4"
    }
}

object DirectedGraphs {
    // 1 -- 2 -- 3
    val linearGraph = DirectedGraph.readInts("1-2,2-3")

    // 1 -- 2   3 -- 4
    val disconnectedGraph = DirectedGraph.readInts("1-2,3-4")

    //   3
    //  / \
    // 2   4
    //  \ /
    //   1
    val diamondGraph = DirectedGraph.readInts("1-2,1-4,2-3,3-4")

    //   3
    //  /|\
    // 2-+-4
    //  \|/
    //   1
    val meshGraph = DirectedGraph.readInts("1-2,1-3,1-4,2-3,2-4,3-4")
}
