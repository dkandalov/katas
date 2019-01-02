package katas.kotlin.hackerrank

import katas.kotlin.skiena.graph2.Edge
import katas.kotlin.skiena.graph2.Graph
import katas.kotlin.skiena.graph2.bfs
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.HashSet

class RoadsAndLibrariesTests {

    private fun <T> Graph<T>.components(): List<Graph<T>> {
        val result = ArrayList<Graph<T>>()
        val visitedVertices = HashSet<T>()
        vertices.forEach { vertex ->
            if (visitedVertices.add(vertex)) {
                val componentVertices = bfs(vertex)
                result.add(Graph(edgesByVertex.filter { componentVertices.contains(it.key) }.toMutableMap()))
                visitedVertices.addAll(componentVertices)
            }
        }
        return result
    }

    private fun <T> Graph<T>.minSpanningTree(): List<Edge<T>> {
        val result = ArrayList<Edge<T>>()

        val queuedVertices = HashSet<T>()
        val queue = LinkedList<T>()

        queuedVertices.add(vertices.first())
        queue.add(vertices.first())

        while (queue.isNotEmpty()) {
            val vertex = queue.removeFirst()
            edgesByVertex[vertex]?.forEach { edge ->
                if (queuedVertices.add(edge.to)) {
                    result.add(edge)
                    queue.add(edge.to)
                }
            }
        }
        return result
    }

    private fun findMinCost(graph: Graph<Int>, libraryCost: Int, roadCost: Int): Int {
        return graph.components().sumBy { component ->
            component.printed()
            val allLibsCost = component.vertices.size * libraryCost
            val allRoadsCost = component.minSpanningTree().printed().size * roadCost + libraryCost
            minOf(allLibsCost, allRoadsCost)
        }
    }

    @Test fun `graph components`() {
        Graph.readInts("1-2,2-3,3-1").components() shouldEqual listOf(Graph.readInts("1-2,2-3,3-1"))
        Graph.readInts("1-2,3-4").components() shouldEqual listOf(Graph.readInts("1-2"), Graph.readInts("3-4"))
    }

    @Test fun `minimum spanning trees`() {
        Graph.readInts("1-2,2-3,3-1").minSpanningTree() shouldEqual listOf(Edge(1, 2), Edge(1, 3))
    }

    @Test fun `example from problem description`() {
        val graph = Graph.readInts("1-2,1-3,1-7,2-3,5-6,6-8")
        findMinCost(graph, libraryCost = 3, roadCost = 2) shouldEqual 16
    }

    @Test fun `first example`() {
        val graph = Graph.readInts("1-2,2-3,3-1")
        findMinCost(graph, libraryCost = 2, roadCost = 1) shouldEqual 4
    }

    @Test fun `second example`() {
        val graph = Graph.readInts("1-2,1-3,2-3,2-4,3-4,5-6")
        findMinCost(graph, libraryCost = 2, roadCost = 5) shouldEqual 12
    }
}