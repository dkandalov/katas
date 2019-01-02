package katas.kotlin.skiena.graph2

import katas.kotlin.skiena.graph2.GraphTest.Companion.diamondGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.disconnectedGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.linearGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.meshGraph
import kotlincommon.doesNotContain
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.ArrayList
import kotlin.collections.HashMap
import kotlin.collections.HashSet
import kotlin.collections.LinkedHashSet

data class SearchResult<T>(
    val prevVertex: Map<T, T>,
    val vertices: LinkedHashSet<T>
)

// This implementation which roughly copies the one from the book
// is bloated with unnecessary complexity so I'll try to avoid it.
fun <T> Graph<T>.bfs_skiena(
    fromVertex: T,
    processVertexEarly: (T) -> Unit = {},
    processVertexLate: (T) -> Unit = {},
    processEdge: (Edge<T>) -> Unit = {}
): SearchResult<T> {
    if (!vertices.contains(fromVertex)) error("Graph doesn't contain vertex '$fromVertex'")

    val prevVertex = HashMap<T, T>()
    val processed = LinkedHashSet<T>()
    val discovered = LinkedHashSet<T>()

    val queue = LinkedList<T>()
    queue.add(fromVertex)
    discovered.add(fromVertex)

    while (queue.isNotEmpty()) {
        val vertex = queue.removeFirst()
        processVertexEarly(vertex)
        processed.add(vertex)

        (edgesByVertex[vertex] ?: emptyList<Edge<T>>())
            .forEach { edge ->
                if (!processed.contains(edge.to)) processEdge(edge)
                if (!discovered.contains(edge.to)) {
                    queue.add(edge.to)
                    discovered.add(edge.to)
                    prevVertex[edge.to] = vertex
                }
            }

        processVertexLate(vertex)
    }

    return SearchResult(prevVertex, processed)
}

fun <T> Graph<T>.bfs(fromVertex: T = vertices.first()): List<T> {
    val result = ArrayList<T>()
    val wasQueued = HashSet<T>().apply { add(fromVertex) }
    val queue = LinkedList<T>().apply { add(fromVertex) }

    while (queue.isNotEmpty()) {
        val vertex = queue.removeFirst()
        result.add(vertex)

        edgesByVertex[vertex]?.map { it.to }
            ?.forEach {
                val justAdded = wasQueued.add(it)
                if (justAdded) queue.add(it)
            }
    }
    return result
}

fun <T> Graph<T>.bfsEdges(fromVertex: T = vertices.first()): List<Edge<T>> {
    val result = ArrayList<Edge<T>>()
    val visited = HashSet<T>()
    val queue = LinkedList<T>().apply { add(fromVertex) }

    while (queue.isNotEmpty()) {
        val vertex = queue.removeFirst()
        visited.add(vertex)

        edgesByVertex[vertex]?.forEach { edge ->
            if (visited.doesNotContain(edge.to)) {
                result.add(edge)
                if (queue.doesNotContain(edge.to)) queue.add(edge.to)
            }
        }
    }
    return result
}

class BFSTests {
    @Test fun `breadth-first search Skiena`() {
        val earlyVertices = ArrayList<Int>()
        val lateVertices = ArrayList<Int>()
        val edges = ArrayList<Edge<Int>>()

        val searchResult = diamondGraph.bfs_skiena(
            fromVertex = 1,
            processVertexEarly = { earlyVertices.add(it) },
            processVertexLate = { lateVertices.add(it) },
            processEdge = { edges.add(it) }
        )

        searchResult.vertices.toList() shouldEqual listOf(1, 2, 4, 3)
        earlyVertices shouldEqual listOf(1, 2, 4, 3)
        lateVertices shouldEqual listOf(1, 2, 4, 3)
        edges shouldEqual listOf(Edge(1, 2), Edge(1, 4), Edge(2, 3), Edge(4, 3))
    }

    @Test fun `breadth-first vertex traversal`() {
        linearGraph.bfs(fromVertex = 1) shouldEqual listOf(1, 2, 3)
        disconnectedGraph.bfs(fromVertex = 1) shouldEqual listOf(1, 2)
        diamondGraph.bfs(fromVertex = 1) shouldEqual listOf(1, 2, 4, 3)
        meshGraph.bfs(fromVertex = 1) shouldEqual listOf(1, 2, 3, 4)
    }

    @Test fun `breadth-first edge traversal`() {
        linearGraph.bfsEdges(fromVertex = 1) shouldEqual listOf(
            Edge(1, 2), Edge(2, 3)
        )
        disconnectedGraph.bfsEdges(fromVertex = 1) shouldEqual listOf(
            Edge(1, 2)
        )
        diamondGraph.bfsEdges(fromVertex = 1) shouldEqual listOf(
            Edge(1, 2), Edge(1, 4), Edge(2, 3), Edge(4, 3)
        )
        meshGraph.bfsEdges(fromVertex = 1) shouldEqual listOf(
            Edge(1, 2), Edge(1, 3), Edge(1, 4), Edge(2, 3), Edge(2, 4), Edge(3, 4)
        )
    }
}