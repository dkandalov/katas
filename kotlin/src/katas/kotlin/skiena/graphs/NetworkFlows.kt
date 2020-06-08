package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.DirectedGraphs.diamondGraph
import katas.kotlin.skiena.graphs.DirectedGraphs.meshGraph
import nonstdlib.join
import nonstdlib.printed
import datsok.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.HashMap

class MaximumNetworkFlowTests {
    @Test fun `maximum network flow in the diamond graph`() {
        FlowGraphs.diamondGraph.let {
            it.maximumFlow(source = "s", sink = "t",isBfs = false).printed() shouldEqual 5
//            it.edges.joinToString("\n") shouldEqual """
//                |s-v|3/3
//                |s-w|2/2
//                |t-v|2/-2
//                |t-w|3/-3
//                |v-w|5/1
//                |v-t|2/2
//                |v-s|3/-3
//                |w-t|3/3
//                |w-s|2/-2
//                |w-v|5/-1
//            """.trimMargin()
        }
    }

    @Test fun `maximum network flow in the diamond2 graph`() {
        FlowGraphs.diamondGraph2.let {
            it.maximumFlow(source = "a", sink = "d", isBfs = false).printed() shouldEqual 20
//            it.edges.joinToString("\n") shouldEqual """
//                |a-b|10/10
//                |a-c|10/10
//                |b-c|1/0
//                |b-d|10/10
//                |b-a|10/-10
//                |c-d|10/10
//                |c-a|10/-10
//                |c-b|1/0
//                |d-b|10/-10
//                |d-c|10/-10
//            """.trimMargin()
        }
    }
}

// See https://en.wikipedia.org/wiki/Edmonds%E2%80%93Karp_algorithm
private fun <T> DirectedGraph<T, FlowEdge<T>>.maximumFlow(source: T, sink: T, isBfs: Boolean = true): Any {
    edges.forEach { edge ->
        val reverseEdge = FlowEdge(edge.to, edge.from, edge.capacity, flow = 0)
        reverseEdge.reverse = edge
        edge.reverse = reverseEdge
        addEdge(reverseEdge)
    }

    var maxFlow = 0
    while (true) {
        val path = if (isBfs) findPathBFS(source, sink) else findPathDFS(source, sink)
        if (path.isEmpty()) break

        val flow = path.map { it.capacity - it.flow }.min()!!
        path.forEach { edge ->
            edge.flow += flow
            // TODO Not sure how reverse flow is supposed to actually work here. Seems to be unnecessary for the above examples.
            edge.reverse!!.flow -= flow
        }
        maxFlow += flow

        println("path = $path")
    }
    return maxFlow
}

private fun <T> DirectedGraph<T, FlowEdge<T>>.findPathDFS(source: T, sink: T): List<FlowEdge<T>> {
    val pathEdges = HashMap<T, FlowEdge<T>>()
    val queue = LinkedList<T>()
    queue.add(source)
    while (queue.isNotEmpty()) {
        val vertex = queue.removeFirst()
        val edge = edgesByVertex[vertex]?.find { edge ->
            pathEdges[edge.to] == null && edge.to != source && edge.capacity > edge.flow
        }
        if (edge != null) {
            pathEdges[edge.to] = edge
            queue.add(edge.to)
        }
    }
    return pathEdges.toReversePathFrom(sink)
}

private fun <T> DirectedGraph<T, FlowEdge<T>>.findPathBFS(source: T, sink: T): List<FlowEdge<T>> {
    val pathEdges = HashMap<T, FlowEdge<T>>()
    val queue = LinkedList<T>()
    queue.add(source)
    while (queue.isNotEmpty()) {
        val vertex = queue.removeFirst()
        edgesByVertex[vertex]?.forEach { edge ->
            if (pathEdges[edge.to] == null && edge.to != source && edge.capacity > edge.flow) {
                pathEdges[edge.to] = edge
                queue.add(edge.to)
            }
        }
    }
    return pathEdges.toReversePathFrom(sink)
}

private fun <T> Map<T, FlowEdge<T>>.toReversePathFrom(vertex: T): List<FlowEdge<T>> {
    val edge = this[vertex] ?: return emptyList()
    return toReversePathFrom(edge.from) + edge
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
    val diamondGraph: DirectedGraph<String, FlowEdge<String>>
        get() = readFlowGraph("s-v|3,s-w|2,v-w|5,v-t|2,w-t|3")

    // From https://en.wikipedia.org/wiki/Ford%E2%80%93Fulkerson_algorithm#Integral_example
    val diamondGraph2: DirectedGraph<String, FlowEdge<String>>
        get() = readFlowGraph("a-b|10,a-c|10,b-c|1,b-d|10,c-d|10")
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
