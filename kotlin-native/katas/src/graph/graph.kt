@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package graph

import kotlin.coroutines.experimental.buildSequence
import kotlin.math.max
import kotlin.test.Test
import kotlin.test.assertEquals


// ------------------------------
// 5.2 Data Structures for Graphs
// ------------------------------

data class EdgeNode(
    var y: Int,
    var weight: Int?,
    var next: EdgeNode?
)

fun EdgeNode?.asIterable(): Iterable<EdgeNode> = buildSequence {
    var edgeNode: EdgeNode? = this@asIterable
    while (edgeNode != null) {
        @Suppress("UNNECESSARY_NOT_NULL_ASSERTION")
        yield(edgeNode!!)
        edgeNode = edgeNode.next
    }
}.asIterable()


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

    private fun insertEdge(x: Int, y: Int) {
        val max = max(x, y) + 1
        if (max > numberOfVertices) numberOfVertices = max
        edges.ensureSize(max, defaultValue = null)
        degree.ensureSize(max, defaultValue = 0)

        edges[x] = EdgeNode(y = y, weight = null, next = edges[x])
        degree[x]++
    }

    override fun toString(): String {
        return edges.indices.flatMap { x ->
            edges[x].asIterable().map { edge ->
                x.toString() + "-" + edge.y
            }
        }.join(",")
    }

    companion object {
        fun read(s: String): Graph {
            val graph = Graph(directed = false)
            s.split(",").forEach {
                val split = it.split("-")
                graph.insertEdge(split[0].toInt(), split[1].toInt())
            }
            return graph
        }
    }
}

class GraphTest {
    @Test fun `create graph from string`() {
        Graph.read("1-2").toString() shouldEqual "1-2"
        Graph.read("2-1").toString() shouldEqual "2-1"

        Graph.read("1-2,2-3").toString() shouldEqual "1-2,2-3"

        // 1──2──4
        // └──3──┘
        Graph.read("1-2,1-3,2-4,3-4").toString() shouldEqual "1-3,1-2,2-4,3-4"
    }
}


// ------------------------
// 5.6 Breadth-First Search
// ------------------------

fun Graph.bfs(
    startVertex: Int,
    processVertexEarly: (Int) -> Unit = {},
    processVertexLate: (Int) -> Unit = {},
    processEdge: (Int, Int) -> Unit = { _, _ -> }
) {
    val discovered = Array(numberOfVertices) { false }
    val processed = Array(numberOfVertices) { false }
    val parents = Array(numberOfVertices) { -1 }

    discovered[startVertex] = true
    parents[startVertex] = -1

    val queue = ArrayList<Int>()
    queue.add(startVertex)

    while (queue.isNotEmpty()) {
        val vertex = queue.removeAt(0)
        processVertexEarly(vertex)
        processed[vertex] = true

        edges[vertex].asIterable().forEach { edge ->
            val y = edge.y
            if (!processed[y] || this.directed) {
                processEdge(vertex, y)
            }
            if (!discovered[y]) {
                queue.add(y)
                discovered[y] = true
                parents[y] = vertex
            }
        }
        processVertexLate(vertex)
    }
}

fun Graph.bfsToList(startVertex: Int): List<Int> {
    val result = ArrayList<Int>()
    bfs(startVertex, processVertexEarly = { result.add(it) })
    return result
}

class BFSTest {
    @Test fun `breadth-first search`() {
        Graph.read("1-2,2-3").bfsToList(1).join(",") shouldEqual "1,2,3"
        // 1──2──4
        // └──3──┘
        Graph.read("1-3,1-2,3-4,2-4").bfsToList(1).join(",") shouldEqual "1,2,3,4"
    }
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

fun <T> T.printed(f: (T) -> String = { it.toString() }): T {
    println(f(this))
    return this
}

fun <T> Iterable<T>.join(
    separator: CharSequence = ", ",
    prefix: CharSequence = "",
    postfix: CharSequence = "",
    limit: Int = -1,
    truncated: CharSequence = "...",
    transform: ((T) -> CharSequence)? = null
): String {
    return this.joinToString(separator, prefix, postfix, limit, truncated, transform)
}