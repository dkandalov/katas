package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.WeightedGraphs.diamondGraph
import katas.kotlin.skiena.graphs.WeightedGraphs.linearGraph
import katas.kotlin.skiena.graphs.WeightedGraphs.triangleGraph
import kotlincommon.doesNotContain
import kotlincommon.test.shouldEqual
import org.junit.Test

class ShortestPathTests {
    @Test fun `Dijkstra shortest paths to all vertices in a graph`() {
        linearGraph.dijkstraShortestPaths() shouldEqual Graph.readInts("1-2/10,2-3/20")
        triangleGraph.dijkstraShortestPaths() shouldEqual Graph.readInts("1-2/20,1-3/20")
        diamondGraph.dijkstraShortestPaths() shouldEqual Graph.readInts("1-2/10,1-4/20,2-3/30")
    }
}

private fun <T> Graph<T>.dijkstraShortestPaths(): Graph<T> {
    val tree = Graph<T>()
    val distance = HashMap<T, Int>()
    tree.addVertex(vertices.first())
    distance[vertices.first()] = 0

    while (!tree.vertices.containsAll(vertices)) {
        val minEdge = tree.vertices
            .flatMap { vertex -> edgesByVertex[vertex]!! }
            .filter { edge -> tree.vertices.doesNotContain(edge.to) }
            .onEach { distance[it.to] = distance[it.from]!! + it.weight!! }
            .minBy { distance[it.to]!! }!!

        tree.addEdge(minEdge.from, minEdge.to, minEdge.weight)
    }

    return tree
}
