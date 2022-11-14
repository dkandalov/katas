package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.WeightedGraphs.diamondGraph
import katas.kotlin.skiena.graphs.WeightedGraphs.linearGraph
import katas.kotlin.skiena.graphs.WeightedGraphs.triangleGraph
import datsok.shouldEqual
import org.junit.Test

class ShortestPathTests {
    @Test fun `Dijkstra shortest paths to all vertices in a graph`() {
        linearGraph.dijkstraShortestPaths() shouldEqual Graph.readInts("1-2/10,2-3/20")
        triangleGraph.dijkstraShortestPaths() shouldEqual Graph.readInts("1-2/20,1-3/20")
        diamondGraph.dijkstraShortestPaths() shouldEqual Graph.readInts("1-2/10,1-4/20,2-3/30")
    }

    @Test fun `Floyd-Warshall shortest paths between all vertices in a graph`() {
        // @formatter:off
        linearGraph.floydWarshallShortestPaths() shouldEqual AllShortestPaths(
            dist = listOf(
                //     1   2   3
                listOf(0,  10, 30), // 1
                listOf(10,  0, 20), // 2
                listOf(30, 20,  0)  // 3
            ),
            next = listOf(
                //      1   2   3
                listOf(-1,  2,  2), // 1
                listOf( 1, -1,  3), // 2
                listOf( 2,  2, -1)  // 3
            )
        ).also {
            it.shortestPath(1, 1) shouldEqual listOf(1)
            it.shortestPath(1, 2) shouldEqual listOf(1, 2)
            it.shortestPath(1, 3) shouldEqual listOf(1, 2, 3)
            it.shortestPath(3, 1) shouldEqual listOf(3, 2, 1)
            it.shortestPath(2, 1) shouldEqual listOf(2, 1)
        }

        triangleGraph.floydWarshallShortestPaths() shouldEqual AllShortestPaths(
            dist = listOf(
                //      1   2   3
                listOf( 0, 20, 20), // 1
                listOf(20,  0, 10), // 2
                listOf(20, 10,  0)  // 3
            ),
            next = listOf(
                //      1   2   3
                listOf(-1,  2,  3), // 1
                listOf( 1, -1,  3), // 2
                listOf( 1,  2, -1)  // 3
            )
        )

        diamondGraph.floydWarshallShortestPaths() shouldEqual AllShortestPaths(
            dist = listOf(
                //      1   2   3   4
                listOf( 0, 10, 40, 20), // 1
                listOf(10,  0, 30, 30), // 2
                listOf(40, 30,  0, 40), // 3
                listOf(20, 30, 40, 0)   // 4
            ),
            next = listOf(
                //      1   2   3   4
                listOf(-1,  2,  2,  4), // 1
                listOf( 1, -1,  3,  1), // 2
                listOf( 2,  2, -1,  4), // 3
                listOf( 1,  1,  3, -1)  // 4
            )
        ).also {
            it.shortestPath(1, 3) shouldEqual listOf(1, 2, 3)
            it.shortestPath(3, 1) shouldEqual listOf(3, 2, 1)
        }
        // @formatter:on
    }
}

private data class AllShortestPaths(val dist: List<List<Int>>, val next: List<List<Int>>) {
    fun shortestPath(from: Int, to: Int): List<Int> {
        if (from == to) return listOf(from)
        return listOf(from) + shortestPath(next[from - 1][to - 1], to)
    }
}

private fun Graph<Int>.floydWarshallShortestPaths(): AllShortestPaths {
    val size = vertices.size + 1 // +1 to make lists indexed from 1
    val dist = MutableList(size) { MutableList(size) { Int.MAX_VALUE / 2 } }
    val next = MutableList(size) { MutableList(size) { -1 } }
    edges.forEach { edge ->
        dist[edge.from][edge.to] = edge.weight!!
        next[edge.from][edge.to] = edge.to
    }
    vertices.forEach {
        dist[it][it] = 0
    }

    vertices.forEach { k ->
        vertices.forEach { i ->
            vertices.forEach { j ->
                if (dist[i][j] > dist[i][k] + dist[k][j]) {
                    dist[i][j] = dist[i][k] + dist[k][j]
                    next[i][j] = next[i][k]
                }
            }
        }
    }

    // make result non-indexed from 1
    dist.removeAt(0)
    dist.forEach { it.removeAt(0) }
    next.removeAt(0)
    next.forEach { it.removeAt(0) }
    return AllShortestPaths(dist, next)
}

private fun <T> Graph<T>.dijkstraShortestPaths(): Graph<T> {
    val tree = Graph<T>()
    val distance = HashMap<T, Int>()
    tree.addVertex(vertices.first())
    distance[vertices.first()] = 0

    while (!tree.vertices.containsAll(vertices)) {
        val minEdge = tree.vertices
            .flatMap { vertex -> edgesByVertex[vertex]!! }
            .filter { edge -> edge.to !in tree.vertices }
            .minBy { edge -> distance.getOrPut(edge.to) { distance[edge.from]!! + edge.weight!! } }

        tree.addEdge(minEdge)
    }
    return tree
}
