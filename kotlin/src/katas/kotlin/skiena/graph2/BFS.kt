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
    processEdge: (T, T) -> Unit = { _, _ -> }
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
            .map { it.toVertex }
            .forEach {
                if (!processed.contains(it)) processEdge(vertex, it)
                if (!discovered.contains(it)) {
                    queue.add(it)
                    discovered.add(it)
                    prevVertex[it] = vertex
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

        val neighbors = edgesByVertex[vertex]?.map { it.toVertex } ?: emptyList()
        queue.addAll(neighbors.filterNot { result.contains(it) })
    }
    return result
}

class BFSTests {
    @Test fun `breadth-first traversal to list`() {
        // 1──2──4
        // └──3──┘
        graphWithCycle.bfs_(fromVertex = 1).toList() shouldEqual listOf(1, 3, 2, 4)
    }

    @Test fun `breadth-first search`() {
        val earlyVertices = ArrayList<Int>()
        val lateVertices = ArrayList<Int>()
        val edges = ArrayList<Pair<Int, Int>>()

        val searchResult = graphWithCycle.bfs(
            fromVertex = 1,
            processVertexEarly = { earlyVertices.add(it) },
            processVertexLate = { lateVertices.add(it) },
            processEdge = { from, to -> edges.add(Pair(from, to)) }
        )

        searchResult.vertices.toList() shouldEqual listOf(1, 3, 2, 4)
        earlyVertices shouldEqual listOf(1, 3, 2, 4)
        lateVertices shouldEqual listOf(1, 3, 2, 4)
        edges shouldEqual listOf(Pair(1, 3), Pair(1, 2), Pair(3, 4), Pair(2, 4))
    }
}