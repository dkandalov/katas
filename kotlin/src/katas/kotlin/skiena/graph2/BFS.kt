package katas.kotlin.skiena.graph2

import katas.kotlin.shouldEqual
import katas.kotlin.skiena.graph2.GraphTest.Companion.graphWithCycle
import org.junit.Test
import java.util.*
import kotlin.collections.ArrayList
import kotlin.collections.HashMap
import kotlin.collections.LinkedHashSet

data class SearchResult<T>(
    val prevVertex: Map<T, T>,
    val vertices: LinkedHashSet<T>
)

fun <T> Graph<T>.bfs(
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

fun <T> Graph<T>.bfs_(fromVertex: T): LinkedHashSet<T> {
    if (!vertices.contains(fromVertex)) error("Graph doesn't contain vertex '$fromVertex'")

    val result = LinkedHashSet<T>()
    val queue = LinkedList<T>()
    queue.add(fromVertex)

    while (queue.isNotEmpty()) {
        val vertex = queue.removeFirst()
        result.add(vertex)

        val neighbors = edgesByVertex[vertex]?.map { it.to } ?: emptyList()
        queue.addAll(neighbors.filterNot { result.contains(it) })
    }
    return result
}

class BFSTests {
    @Test fun `breadth-first traversal to list`() {
        // 1──2──4
        // └──3──┘
        graphWithCycle.bfs_(fromVertex = 1).toList() shouldEqual listOf(1, 2, 3, 4)
    }

    @Test fun `breadth-first search`() {
        val earlyVertices = ArrayList<Int>()
        val lateVertices = ArrayList<Int>()
        val edges = ArrayList<Edge<Int>>()

        // 1──2──4
        // └──3──┘
        val searchResult = graphWithCycle.bfs(
            fromVertex = 1,
            processVertexEarly = { earlyVertices.add(it) },
            processVertexLate = { lateVertices.add(it) },
            processEdge = { edges.add(it) }
        )

        searchResult.vertices.toList() shouldEqual listOf(1, 2, 3, 4)
        earlyVertices shouldEqual listOf(1, 2, 3, 4)
        lateVertices shouldEqual listOf(1, 2, 3, 4)
        edges shouldEqual listOf(Edge(1, 2), Edge(1, 3), Edge(2, 4), Edge(3, 4))
    }
}