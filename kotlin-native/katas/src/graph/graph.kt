@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package graph

import graph.Colour.*
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
        val processedEdgeFrom = Array(numberOfVertices) { false }
        val processedEdgeTo = Array(numberOfVertices) { false }
        return edges.indices.flatMap { x ->
            edges[x].asIterable()
                .filterNot { edge -> processedEdgeFrom[edge.y] && processedEdgeTo[x] }
                .map { edge ->
                    processedEdgeFrom[x] = true
                    processedEdgeTo[edge.y] = true
                    x.toString() + "-" + edge.y
                }
        }.join(",")
    }

    companion object {
        fun read(s: String): Graph {
            val graph = Graph(directed = false)
            s.split(",").forEach {
                val split = it.split("-")
                val x = split[0].toInt()
                val y = split[1].toInt()
                graph.insertEdge(x, y)
                if (!graph.directed) graph.insertEdge(y, x)
            }
            return graph
        }
    }
}

class GraphTest {
    @Test fun `create graph from string`() {
        Graph.read("1-2").toString() shouldEqual "1-2"
        Graph.read("2-1").toString() shouldEqual "1-2"

        Graph.read("1-2,2-3").toString() shouldEqual "1-2,2-3"

        // 1──2──4
        // └──3──┘
        Graph.read("1-2,1-3,2-4,3-4").toString() shouldEqual "1-3,1-2,2-4,3-4"
    }
}


// ------------------------
// 5.6 Breadth-First Search
// ------------------------

// 1──2──3
// |  |  |
// ├──5──4
// └──6
// From Figure 5.9
val exampleGraph = Graph.read("1-2,2-3,3-4,4-5,2-5,1-5,1-6")

class BfsResult(
    val parents: Array<Int>,
    val discovered: Array<Boolean>
)

fun Graph.bfs(
    startVertex: Int,
    processVertexEarly: (Int) -> Unit = {},
    processVertexLate: (Int) -> Unit = {},
    processEdge: (Int, Int) -> Unit = { _, _ -> }
): BfsResult {
    val discovered = Array(numberOfVertices) { false }
    val processed = Array(numberOfVertices) { false }
    val parents = Array(numberOfVertices) { -1 }

    discovered[startVertex] = true
    parents[startVertex] = -1

    val queue = ArrayList<Int>().apply { add(startVertex) }

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

    return BfsResult(parents, discovered)
}

fun Graph.bfsToList(startVertex: Int): List<Int> {
    val result = ArrayList<Int>()
    bfs(startVertex, processVertexEarly = { result.add(it) })
    return result
}

class BfsTest {
    @Test fun `breadth-first graph traversal`() {
        Graph.read("1-2,2-3").bfsToList(1).join(",") shouldEqual "1,2,3"

        // 1──2──4
        // └──3──┘
        Graph.read("1-3,1-2,3-4,2-4").bfsToList(1).join(",") shouldEqual "1,2,3,4"

        exampleGraph.bfsToList(1).join(",") shouldEqual "1,6,5,2,4,3"
    }
}

// -------------------
// 5.6.2 Finding Paths
// -------------------
fun findPath(start: Int, end: Int, parents: Array<Int>, result: List<Int> = ArrayList()): List<Int> {
    return if (start == end || end == -1) result + start
    else findPath(start, parents[end], parents) + end
}

class FindingPathsTest {
    @Test fun `find path`() {
        val parents = exampleGraph.bfs(1).parents
        findPath(1, 4, parents).join() shouldEqual "1, 5, 4"
    }
}

// --------------------------
// 5.7.1 Connected Components
// --------------------------

typealias Component = List<Int>

fun Graph.connectedComponents(): List<Component> {
    val result = ArrayList<Component>()
    var discovered = Array(numberOfVertices) { false }
    0.until(numberOfVertices).forEach { i ->
        if (!discovered[i]) {
            val component = ArrayList<Int>()
            discovered = bfs(i, processVertexEarly = { component.add(it) }).discovered
            result.add(component)
        }
    }
    return result
}

class ConnectedComponentsTest {
    @Test fun `find connected components in a graph`() {
        Graph.read("0-1,1-2").connectedComponents() shouldEqual listOf(listOf(0, 1, 2))
        Graph.read("0-1,2-3").connectedComponents() shouldEqual listOf(listOf(0, 1), listOf(2, 3))
    }
}

// -------------------------
// 5.7.2 Two-Coloring Graphs
// -------------------------

enum class Colour {
    uncolored, white, black;

    fun complement() = when (this) {
        white -> black
        black -> white
        else -> uncolored
    }
}

fun Graph.twoColour(): Array<Colour>? {
    val colours = Array(numberOfVertices) { uncolored }
    var discovered = Array(numberOfVertices) { false }
    var bipartite = true

    val edgeHandler = { x: Int, y: Int ->
        if (colours[x] == colours[y]) {
            bipartite = false
        }
        colours[y] = colours[x].complement()
    }

    0.until(numberOfVertices).forEach { i ->
        if (!discovered[i]) {
            colours[i] = white
            discovered = bfs(i, processEdge = edgeHandler).discovered
        }
    }
    return if (bipartite) colours else null
}

class TwoColourTest {
    @Test fun `assign two colours to graph vertexes`() {
        Graph.read("0-1,1-2").twoColour()?.toList() shouldEqual listOf(white, black, white)
        Graph.read("0-1,1-2,0-2").twoColour() shouldEqual null
    }
}


// ----------------------
// 5.8 Depth-First Search
// ----------------------

class DfsState(
    val discovered: Array<Boolean>,
    val processed: Array<Boolean>,
    val parents: Array<Int>,
    val entryTime: Array<Int>,
    val exitTime: Array<Int>
) {
    constructor(numberOfVertices: Int) : this(
        Array(numberOfVertices) { false },
        Array(numberOfVertices) { false },
        Array(numberOfVertices) { -1 },
        Array(numberOfVertices) { 0 },
        Array(numberOfVertices) { 0 }
    )
}

fun Graph.dfs(
    vertex: Int,
    state: DfsState = DfsState(numberOfVertices),
    processVertexEarly: (Int) -> Unit = {},
    processVertexLate: (Int) -> Unit = {},
    processEdge: (Int, Int) -> Unit = { _, _ -> }
): DfsState = state.run {
    var time = 0

    discovered[vertex] = true
    entryTime[vertex] = ++time

    processVertexEarly(vertex)

    edges[vertex].asIterable().forEach { edge ->
        val y = edge.y
        if (!discovered[y]) {
            parents[y] = vertex
            processEdge(vertex, y)
            dfs(y, state, processVertexEarly, processVertexLate, processEdge)
        } else if (!processed[vertex] || directed) {
            processEdge(vertex, y)
        }
    }

    processVertexLate(vertex)

    exitTime[vertex] = ++time
    processed[vertex] = true

    return state
}

fun Graph.dfsToList(startVertex: Int): List<Int> {
    val result = ArrayList<Int>()
    dfs(startVertex, processVertexEarly = { result.add(it) })
    return result
}

class DfsTest {
    @Test fun `depth-first graph traversal`() {
        Graph.read("0-1,1-2").dfsToList(0) shouldEqual listOf(0, 1, 2)
        Graph.read("0-1,1-2").dfsToList(2) shouldEqual listOf(2, 1, 0)
        Graph.read("0-3,0-1,1-2").dfsToList(0) shouldEqual listOf(0, 1, 2, 3)
    }
}

// -------------------
// Util
// -------------------

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