package katas.kotlin.skiena.graph2

import katas.kotlin.skiena.graph2.GraphTest.Companion.diamondGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.disconnectedGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.linearGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.meshGraph
import kotlincommon.doesNotContain
import kotlincommon.test.shouldEqual
import org.junit.Test

fun <T> Graph<T>.countComponents(): Int {
    var result = 0
    val visitedVertices = HashSet<T>()
    vertices.forEach { vertex ->
        if (visitedVertices.doesNotContain(vertex)) {
            visitedVertices.addAll(bfs(vertex))
            result++
        }
    }
    return result
}

fun <T> Graph<T>.twoColor(): Map<T, Boolean> {
    val result = HashMap<T, Boolean>()
    vertices.forEach { vertex ->
        if (!result.containsKey(vertex)) {
            bfsEdges(vertex).forEach { edge ->
                val fromColor = result[edge.from]
                val toColor = result[edge.to]
                if (fromColor == null) {
                    result[edge.from] = true
                    result[edge.to] = false
                }
                else if (fromColor == toColor) return emptyMap()
                else result[edge.to] = !fromColor
            }
        }
    }
    return result
}

class BFSApplicationTests {
    @Test fun `count components`() {
        Graph.readInts("1-2").countComponents() shouldEqual 1
        Graph.readInts("1-2,2-3").countComponents() shouldEqual 1
        Graph.readInts("1-2,3-4").countComponents() shouldEqual 2
        Graph.readInts("1-2,2-3,5-6").countComponents() shouldEqual 2
        Graph.readInts("1-2,3-4,5-6").countComponents() shouldEqual 3

        diamondGraph.countComponents() shouldEqual 1
        meshGraph.countComponents() shouldEqual 1
    }

    @Test fun `color bipartite graph`() {
        Graph.readInts("1-2").twoColor() shouldEqual mapOf(
            1 to true,
            2 to false
        )
        linearGraph.twoColor() shouldEqual mapOf(
            1 to true,
            2 to false,
            3 to true
        )
        disconnectedGraph.twoColor() shouldEqual mapOf(
            1 to true,
            2 to false,
            3 to true,
            4 to false
        )
        diamondGraph.twoColor() shouldEqual mapOf(
            1 to true,
            2 to false,
            3 to false,
            4 to true
        )
        meshGraph.twoColor() shouldEqual emptyMap()
    }
}