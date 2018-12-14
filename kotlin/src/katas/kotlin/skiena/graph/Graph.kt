package katas.kotlin.skiena.graph

import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test
import katas.kotlin.skiena.graph.Colour.*

// ------------------------------
// 5.2 Data Structures for Graphs
// ------------------------------

data class EdgeNode(
    var y: Int,
    var weight: Int?,
    var next: EdgeNode?
) : Iterable<EdgeNode> {
    override fun iterator(): Iterator<EdgeNode> =
        generateSequence(this) { edgeNode -> edgeNode.next }.iterator()
}


class Graph(
    var edges: ArrayList<EdgeNode?>,
    var degree: ArrayList<Int>,
    var numberOfVertices: Int = 0,
    var numberOfEdges: Int = 0,
    val directed: Boolean
) {
    constructor(directed: Boolean) : this(
        edges = ArrayList<EdgeNode?>(),
        degree = ArrayList<Int>(),
        directed = directed
    )

    private fun insertEdge(x: Int, y: Int) {
        val max = maxOf(x, y) + 1
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
            (edges[x] ?: emptyList<EdgeNode>())
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

        edges[vertex]!!.forEach { edge ->
            val targetVertex = edge.y
            if (!processed[targetVertex] || directed) {
                processEdge(vertex, targetVertex)
            }
            if (!discovered[targetVertex]) {
                queue.add(targetVertex)
                discovered[targetVertex] = true
                parents[targetVertex] = vertex
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
        else  -> uncolored
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
    val exitTime: Array<Int>,
    var time: Int = 0,
    var finished: Boolean = false
) {
    constructor(numberOfVertices: Int) : this(
        discovered = Array(numberOfVertices) { false },
        processed = Array(numberOfVertices) { false },
        parents = Array(numberOfVertices) { -1 },
        entryTime = Array(numberOfVertices) { 0 },
        exitTime = Array(numberOfVertices) { 0 }
    )
}

fun Graph.dfs(
    vertex: Int,
    state: DfsState = DfsState(numberOfVertices),
    processVertexEarly: (Int) -> Unit = {},
    processVertexLate: (Int) -> Unit = {},
    processEdge: (Int, Int, DfsState) -> Unit = { _, _, _ -> }
): DfsState = state.run {
    if (finished) return state

    discovered[vertex] = true
    entryTime[vertex] = ++time

    processVertexEarly(vertex)

    edges[vertex]!!.forEach { edge ->
        val y = edge.y
        if (!discovered[y]) {
            parents[y] = vertex
            processEdge(vertex, y, state)
            dfs(y, state, processVertexEarly, processVertexLate, processEdge)
        } else if ((!processed[y] && parents[vertex] != y) || directed) {
            processEdge(vertex, y, state)
        }
        if (finished) return state
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


// --------------------
// 5.9.1 Finding Cycles
// --------------------

fun Graph.findCycle(startVertex: Int = 0): Pair<Int, Int>? {
    var result: Pair<Int, Int>? = null
    dfs(startVertex, processEdge = { x, y, state ->
        if (state.discovered[y] && state.parents[x] != y) {
            result = Pair(y, x)
            state.finished = true
        }
    })
    return result
}

class FindCyclesTest {
    @Test fun `find cycle in a graph`() {
        Graph.read("0-1,1-2").findCycle() shouldEqual null
        Graph.read("0-1,1-0").findCycle() shouldEqual null
        Graph.read("0-1,1-2,2-0").findCycle() shouldEqual Pair(0, 1)

        // 1──2──4
        // └──3──┘
        Graph.read("1-3,1-2,3-4,2-4").findCycle(1) shouldEqual Pair(1, 3)
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
