@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package graph

import graph.BfsState.*
import kotlin.coroutines.experimental.buildSequence
import kotlin.test.Test
import kotlin.test.assertEquals


// ------------------------------
// 5.2 Data Structures for Graphs
// ------------------------------

data class EdgeNode(
    var y: Int,
    var weight: Int?,
    var next: EdgeNode?
) {
    fun iterate(): Iterable<EdgeNode> {
        return buildSequence {
            var edgeNode: EdgeNode? = this@EdgeNode
            while (edgeNode != null) {
                @Suppress("UNNECESSARY_NOT_NULL_ASSERTION")
                yield(edgeNode!!)
                edgeNode = edgeNode.next
            }
        }.asIterable()
    }
}

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
        edges.ensureSize(x + 1, defaultValue = null)
        degree.ensureSize(x + 1, defaultValue = 0)

        edges[x] = EdgeNode(y = y, weight = null, next = edges[x])
        degree[x]++
    }

    override fun toString(): String {
        return edges.indices.map { x ->
            edges[x]?.iterate()?.joinToString(",") { edge ->
                x.toString() + "-" + edge.y
            } ?: ""
        }.filter { it.isNotEmpty() }.joinToString(",")
    }

    companion object {
        fun read(s: String): Graph {
            val graph = Graph(directed = false)
            val vertices = HashMap<Int, Unit>() // use map because clion can't process HashSet at the moment :(
            s.split(",").forEach {
                val (x, y) = it.split("-").let { Pair(it[0].toInt(), it[1].toInt()) }
                graph.insertEdge(x, y)
                vertices[x] = Unit
                vertices[y] = Unit
                graph.numberOfVertices = vertices.size
            }
            return graph
        }
    }
}

class GraphTest {
    @Test
    fun `create graph from string`() {
        Graph.read("1-2").toString() shouldEqual "1-2"
        Graph.read("2-1").toString() shouldEqual "2-1"

        Graph.read("1-2,2-3").toString() shouldEqual "1-2,2-3"

        // 1──2──4
        // └──3──┘
        Graph.read("1-2,1-3,2-4,3-4").toString() shouldEqual "1-3,1-2,2-4,3-4"
    }

    @Test
    fun `breadth-first search`() {
        // 1──2──4
        // └──3──┘
        // TODO Graph.read("1-2,1-3,2-4,3-4").bfs(1).joinToString() shouldEqual "1,2,3,4"
    }
}


// ------------------------
// 5.6 Breadth-First Search
// ------------------------

private enum class BfsState {
    undiscovered, discovered, processed
}

fun Graph.bfs(s: Int): List<Int> {
    val state = Array(numberOfVertices) { undiscovered }
    val parents = Array(numberOfVertices) { -1 }

    state[s] = discovered
    parents[s] = -1

    val queue = ArrayList<Int>()
    queue.add(s)

//    while (queue.isNotEmpty()) {
//        val head = queue.removeAt(0)
//        println(head) // TODO
//        //edges[head]
//        queue.add(123)
//    }
    return emptyList()
}

class BFSTest {
    @Test
    fun foo() {
        // TODO
    }
}

fun <T> ArrayList<T>.fillWith(value: T, size: Int): ArrayList<T> {
    0.until(size).forEach { add(value) }
    return this
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
