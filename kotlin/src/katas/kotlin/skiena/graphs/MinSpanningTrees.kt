package katas.kotlin.skiena.graphs

import kotlincommon.doesNotContain
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import java.util.Comparator.comparingInt

class MinimumSpanningTreeTests {
    private val linearGraph = Graph.readInts("1-2/10,2-3/20")

    //   3
    //  / \
    // 2   4
    //  \ /
    //   1
    private val diamondGraph = Graph.readInts("1-2/10,1-4/20,2-3/30,3-4/40")

    // From Skiena Figure 6.3
    private val exampleGraph = Graph.read("A-B/5,A-C/7,A-D/12,B-C/9,C-D/4,B-E/7,C-E/4,C-F/3,E-F/2,E-G/5,F-G/2")

    @Test fun `Prim's min spanning tree`() {
        linearGraph.primMST() shouldEqual linearGraph
        diamondGraph.primMST() shouldEqual Graph.readInts("1-2/10,1-4/20,2-3/30")
        exampleGraph.primMST() shouldEqual Graph.read("A-B/5,A-C/7,C-F/3,C-D/4,E-F/2,F-G/2")
    }

    @Test fun `Kruskal's min spanning tree`() {
        linearGraph.kruskalMST() shouldEqual linearGraph
        diamondGraph.kruskalMST() shouldEqual Graph.readInts("1-2/10,1-4/20,2-3/30")
        exampleGraph.kruskalMST() shouldEqual Graph.read("A-B/5,A-C/7,C-F/3,C-D/4,E-F/2,F-G/2")
    }
}

private fun <T> Graph<T>.kruskalMST(): Graph<T> {
    val treeByVertex = HashMap<T, Graph<T>>()
    vertices.forEach {
        val graph = Graph<T>()
        graph.addVertex(it)
        treeByVertex[it] = graph
    }

    val queue = PriorityQueue<Edge<T>>(comparingInt { it.weight!! })
    queue.addAll(edgesByVertex.values.flatten())

    while (queue.isNotEmpty()) {
        val (from, to, weight) = queue.remove()
        val fromTree = treeByVertex[from]!!
        val toTree = treeByVertex[to]!!

        if (fromTree != toTree) {
            fromTree.edgesByVertex.putAll(toTree.edgesByVertex)
            fromTree.addEdge(from, to, weight)
            toTree.vertices.forEach {
                treeByVertex[it] = fromTree
            }
        }
    }
    return treeByVertex.values.first()
}

private fun <T> Graph<T>.primMST(): Graph<T> {
    val tree = Graph<T>()
    tree.addVertex(vertices.first())

    while (!tree.vertices.containsAll(vertices)) {
        val minEdge = tree.vertices
            .flatMap { vertex -> edgesByVertex[vertex]!! }
            .filter { edge -> tree.vertices.doesNotContain(edge.to) }
            .minBy { it.weight!! }!!

        tree.addEdge(minEdge.from, minEdge.to, minEdge.weight)
    }

    return tree
}
