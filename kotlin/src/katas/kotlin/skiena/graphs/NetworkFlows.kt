package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.DirectedGraphs.diamondGraph
import katas.kotlin.skiena.graphs.DirectedGraphs.meshGraph
import kotlincommon.join
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*

class MaximumNetworkFlowTests {
    @Test fun `find maximum network flow in the diamond graph`() {
        FlowGraphs.diamondGraph.maximumFlow(source = "s", sink = "t").printed()
    }
}

// See https://en.wikipedia.org/wiki/Edmonds%E2%80%93Karp_algorithm
private fun <T> DirectedGraph<T, FlowEdge<T>>.maximumFlow(source: T, sink: T): Any {
    edges.forEach { edge ->
        val reverseEdge = FlowEdge(edge.to, edge.from, edge.capacity, edge.flow)
        reverseEdge.reverse = edge
        edge.reverse = reverseEdge
        addEdge(reverseEdge)
    }

    var flow = 0
    while (true) {
        val path = ArrayList<FlowEdge<T>>()
        val queue = LinkedList<T>()
        queue.add(source)
        while (queue.isNotEmpty()) {
            val vertex = queue.removeFirst()
            println("vertex = ${vertex}")
            edgesByVertex[vertex]?.forEach { edge ->
                if (path.none { it.to == edge.to } && edge.to != source && edge.capacity > edge.flow) {
                    path.add(edge)
                    queue.add(edge.to)
                }
            }
        }
        if (path.lastOrNull()?.to != sink) {
            println("break")
            println(this)
            break
        }

        var minFlow = Int.MAX_VALUE
        path.forEach {
            minFlow = minOf(minFlow, it.capacity - it.flow)
        }
        path.forEach {
            it.flow += minFlow
            it.reverse!!.flow -= minFlow
        }
        flow += minFlow

        println("path = ${path}")
    }
    return flow
}


data class FlowEdge<T>(
    override val from: T,
    override val to: T,
    val capacity: Int,
    var flow: Int
): EdgeType<T> {
    var reverse: FlowEdge<T>? = null

    override fun toString() = "$from-$to|$capacity/$flow"

    companion object {
        fun <T> read(token: String, parse: (String) -> T): FlowEdge<T>? {
            val split = token.split('-', '|', '/')
            return FlowEdge(
                from = parse(split[0]),
                to = if (split.size >= 2) parse(split[1]) else return null,
                capacity = split[2].toInt(),
                flow = if (split.size >= 4) split[3].toInt() else 0
            )
        }
    }
}

fun readFlowGraph(s: String): DirectedGraph<String, FlowEdge<String>> {
    val graph = DirectedGraph<String, FlowEdge<String>>()
    s.split(",").forEach { token ->
        val edge = FlowEdge.read(token, parse = { it })
        if (edge != null) graph.addEdge(edge) else graph.addVertex(token)
    }
    return graph
}

class FlowGraphTests {
    @Test fun `create flow graph from string`() {
        FlowGraphs.diamondGraph.toString() shouldEqual
            "s-v|3/0," +
            "s-w|2/0," +
            "v-w|5/0," +
            "v-t|2/0," +
            "w-t|3/0"
    }
}

object FlowGraphs {
    // From https://cs.stackexchange.com/questions/55041/residual-graph-in-maximum-flow
    val diamondGraph = readFlowGraph("s-v|3,s-w|2,v-w|5,v-t|2,w-t|3")
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

interface EdgeType<T> {
    val from: T
    val to: T
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

class DirectedGraphTests {
    @Test fun `create directed graph from string`() {
        DirectedGraph.readInts("1-2").toString() shouldEqual "1-2"
        DirectedGraph.readInts("2-1").toString() shouldEqual "2-1"
        DirectedGraph.readInts("1-2,2-3").toString() shouldEqual "1-2,2-3"
        DirectedGraph.readInts("1-2,3").toString() shouldEqual "1-2,3"

        diamondGraph.toString() shouldEqual "1-2,1-4,2-3,3-4"
        meshGraph.toString() shouldEqual "1-2,1-3,1-4,2-3,2-4,3-4"
    }
}

object DirectedGraphs {
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
