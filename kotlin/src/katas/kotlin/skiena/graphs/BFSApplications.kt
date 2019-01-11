package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.GraphTest.Companion.diamondGraph
import katas.kotlin.skiena.graphs.GraphTest.Companion.disconnectedGraph
import katas.kotlin.skiena.graphs.GraphTest.Companion.linearGraph
import katas.kotlin.skiena.graphs.GraphTest.Companion.meshGraph
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.ArrayList
import kotlin.collections.HashMap
import kotlin.collections.HashSet
import kotlin.collections.set


fun <T> Graph<T>.components(): List<Graph<T>> {
    val result = ArrayList<Graph<T>>()
    val visitedVertices = HashSet<T>()
    vertices.forEach { vertex ->
        if (visitedVertices.add(vertex)) {
            val componentVertices = bfs(vertex)
            val componentGraph = Graph(HashMap(edgesByVertex).apply { keys.retainAll(componentVertices) })
            result.add(componentGraph)
            visitedVertices.addAll(componentVertices)
        }
    }
    return result
}

fun <T> Graph<T>.countComponents(): Int = components().size

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
                } else if (fromColor == toColor) return emptyMap()
                else result[edge.to] = !fromColor
            }
        }
    }
    return result
}

class BFSApplicationTests {

    @Test fun `graph components`() {
        linearGraph.components() shouldEqual listOf(linearGraph)
        disconnectedGraph.components() shouldEqual listOf(Graph.readInts("1-2"), Graph.readInts("3-4"))
        diamondGraph.components() shouldEqual listOf(diamondGraph)
    }

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
            3 to true,
            4 to false
        )
        meshGraph.twoColor() shouldEqual emptyMap()
    }
}