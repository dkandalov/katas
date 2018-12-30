package katas.kotlin.skiena.graph2

import katas.kotlin.skiena.graph2.GraphTest.Companion.diamondGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.linearGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.meshGraph
import kotlincommon.test.shouldEqual
import org.junit.Test

fun <T> Graph<T>.hasCycles(): Boolean {
    val fromVertex = edgesByVertex.keys.first()
    val visited = HashSet<T>().apply { add(fromVertex) }
    dfsEdges(fromVertex).forEach { edge ->
        val justAdded = visited.add(edge.to)
        if (!justAdded) return true
    }
    return false
}

fun <T> Graph<T>.findCutVertices(
    vertex: T = vertices.first(),
    visited: MutableSet<T> = HashSet<T>().apply { add(vertex) },
    parent: MutableMap<T, T> = HashMap(),
    depth: MutableMap<T, Int> = HashMap<T, Int>().apply { put(vertex, 1) },
    low: MutableMap<T, Int> = HashMap<T, Int>().apply { put(vertex, 1) },
    result: MutableList<T> = ArrayList()
): List<T> {
    var isCutVertex = false
    var childCount = 0
    edgesByVertex[vertex]?.forEach { (from, to) ->
        val justAdded = visited.add(to)
        if (justAdded) {
            childCount++
            parent[to] = from
            depth[to] = depth[from]!! + 1
            low[to] = depth[to]!!

            findCutVertices(to, visited, parent, depth, low, result)

            if (low[to]!! >= depth[from]!!) isCutVertex = true
            low[from] = minOf(low[from]!!, low[to]!!)
        } else if (to != parent[from]) {
            low[from] = minOf(low[from]!!, depth[to]!!)
        }
    }
    if (parent[vertex] != null && isCutVertex) result.add(vertex)
    if (parent[vertex] == null && childCount > 1) result.add(vertex)


    return result
}

class DFSApplicationsTests {
    @Test fun `find cycles in an undirected graph`() {
        linearGraph.hasCycles() shouldEqual false
        diamondGraph.hasCycles() shouldEqual true
        meshGraph.hasCycles() shouldEqual true
    }

    @Test fun `find cut vertices of undirected graph`() {
        linearGraph.findCutVertices() shouldEqual listOf(2)
        diamondGraph.findCutVertices() shouldEqual emptyList()
        meshGraph.findCutVertices() shouldEqual emptyList()

        //   2
        //  / \
        // 1   3--5--6
        //  \ /
        //   4
        Graph.readInts("1-2,1-4,2-3,3-4,3-5,5-6").findCutVertices() shouldEqual listOf(5, 3)

        // 2--3--6
        // |  |  |
        // 1--4--5
        Graph.readInts("1-2,1-4,2-3,3-4,4-5,5-6,6-3").findCutVertices() shouldEqual emptyList()

        // 2--3--4
        // |  |  |
        // 1--6--5
        Graph.readInts("1-2,1-6,2-3,3-4,4-5,5-6,3-6").findCutVertices() shouldEqual emptyList()
    }
}
