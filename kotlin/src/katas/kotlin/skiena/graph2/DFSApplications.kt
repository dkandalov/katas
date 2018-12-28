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

class DFSApplicationsTests {
    @Test fun `find cycles in an undirected graph`() {
        linearGraph.hasCycles() shouldEqual false
        diamondGraph.hasCycles() shouldEqual true
        meshGraph.hasCycles() shouldEqual true
    }
}