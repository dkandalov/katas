package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.GraphTest.Companion.diamondGraph
import katas.kotlin.skiena.graphs.GraphTest.Companion.disconnectedGraph
import katas.kotlin.skiena.graphs.GraphTest.Companion.linearGraph
import katas.kotlin.skiena.graphs.GraphTest.Companion.meshGraph
import kotlincommon.doesNotContain
import kotlincommon.test.shouldEqual
import org.junit.Test

fun <T> Graph<T>.dfs(fromVertex: T, result: MutableList<T> = ArrayList()): List<T> {
    edgesByVertex[fromVertex]?.forEach {
        if (result.doesNotContain(it.from)) result.add(it.from)
        if (result.doesNotContain(it.to)) {
            result.add(it.to)
            dfs(it.to, result)
        }
    }
    return result
}

fun <T> Graph<T>.dfsEdges(fromVertex: T, result: MutableList<Edge<T>> = ArrayList()): List<Edge<T>> {
    edgesByVertex[fromVertex]?.forEach { edge ->
        if (result.doesNotContain(edge) && result.doesNotContain(edge.inverse)) {
            result.add(edge)
            dfsEdges(edge.to, result)
        }
    }
    return result
}

private val <T> Edge<T>.inverse: Edge<T> get() = Edge(to, from)

class DFSTests {

    @Test fun `depth-first vertex traversal`() {
        linearGraph.dfs(fromVertex = 1) shouldEqual listOf(1, 2, 3)
        disconnectedGraph.dfs(fromVertex = 1) shouldEqual listOf(1, 2)
        diamondGraph.dfs(fromVertex = 1) shouldEqual listOf(1, 2, 3, 4)
        meshGraph.dfs(fromVertex = 1) shouldEqual listOf(1, 2, 3, 4)
    }

    @Test fun `depth-first edge traversal`() {
        linearGraph.dfsEdges(fromVertex = 1) shouldEqual listOf(
            Edge(1, 2), Edge(2, 3)
        )
        disconnectedGraph.dfsEdges(fromVertex = 1) shouldEqual listOf(
            Edge(1, 2)
        )
        diamondGraph.dfsEdges(fromVertex = 1) shouldEqual listOf(
            Edge(1, 2), Edge(2, 3), Edge(3, 4), Edge(4, 1)
        )
        meshGraph.dfsEdges(fromVertex = 1) shouldEqual listOf(
            Edge(1, 2), Edge(2, 3), Edge(3, 1), Edge(1, 4), Edge(4, 2), Edge(4, 3)
        )
    }
}