package katas.kotlin.skiena.graph2

import katas.kotlin.skiena.graph2.GraphTest.Companion.diamondGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.disconnectedGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.linearGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.meshGraph
import kotlincommon.test.shouldEqual
import org.junit.Test

fun <T> Graph<T>.dfs(fromVertex: T, result: MutableList<T> = ArrayList()): List<T> {
    if (result.contains(fromVertex)) return result
    result.add(fromVertex)
    edgesByVertex[fromVertex]?.forEach {
        dfs(it.to, result)
    }
    return result
}

class DFSTests {
    @Test fun `depth-first vertex traversal`() {
        linearGraph.dfs(fromVertex = 1) shouldEqual listOf(1, 2, 3)
        disconnectedGraph.dfs(fromVertex = 1) shouldEqual listOf(1, 2)
        diamondGraph.dfs(fromVertex = 1) shouldEqual listOf(1, 2, 3, 4)
        meshGraph.dfs(fromVertex = 1) shouldEqual listOf(1, 2, 3, 4)
    }
}