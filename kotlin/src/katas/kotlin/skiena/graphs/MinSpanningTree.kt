package katas.kotlin.skiena.graphs

import katas.kotlin.skiena.graphs.WeightedGraphs.diamondGraph
import katas.kotlin.skiena.graphs.WeightedGraphs.linearGraph
import katas.kotlin.skiena.graphs.WeightedGraphs.exampleGraph
import katas.kotlin.skiena.graphs.WeightedGraphs.triangleGraph
import kotlincommon.doesNotContain
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import java.util.Comparator.comparingInt

object WeightedGraphs {
    val linearGraph = Graph.readInts("1-2/10,2-3/20")

    // 2 -- 3
    //  \  /
    //   1
    val triangleGraph = Graph.readInts("1-2/20,1-3/20,2-3/10")

    //   3
    //  / \
    // 2   4
    //  \ /
    //   1
    val diamondGraph = Graph.readInts("1-2/10,1-4/20,2-3/30,3-4/40")

    // From Skiena Figure 6.3
    val exampleGraph = Graph.read("A-B/5,A-C/7,A-D/12,B-C/9,C-D/4,B-E/7,C-E/4,C-F/3,E-F/2,E-G/5,F-G/2")
}

class MinimumSpanningTreeTests {
    @Test fun `Prim's min spanning tree`() {
        linearGraph.primMST() shouldEqual linearGraph
        triangleGraph.primMST() shouldEqual Graph.readInts("1-2/20,2-3/10")
        diamondGraph.primMST() shouldEqual Graph.readInts("1-2/10,1-4/20,2-3/30")
        exampleGraph.primMST() shouldEqual Graph.read("A-B/5,A-C/7,C-F/3,C-D/4,E-F/2,F-G/2")
    }

    @Test fun `Kruskal's min spanning tree`() {
        linearGraph.kruskalMST() shouldEqual linearGraph
        triangleGraph.kruskalMST() shouldEqual Graph.readInts("1-3/20,2-3/10")
        diamondGraph.kruskalMST() shouldEqual Graph.readInts("1-2/10,1-4/20,2-3/30")
        exampleGraph.kruskalMST() shouldEqual Graph.read("A-B/5,A-C/7,C-F/3,C-D/4,E-F/2,F-G/2")
    }
}

class DisjointSetTests {
    @Test fun `connect 1 and 2`() {
        val set = DisjointSet(listOf(1, 2))
        set.find(1) shouldEqual 1
        set.find(2) shouldEqual 2
        set.areConnected(1, 2) shouldEqual false

        set.join(1, 2)
        set.find(1) shouldEqual 1
        set.find(2) shouldEqual 1
        set.areConnected(1, 2) shouldEqual true
    }

    @Test fun `connect 1, 2, 3`() {
        val set = DisjointSet(listOf(1, 2, 3))
        set.areConnected(1, 2) shouldEqual false
        set.areConnected(1, 3) shouldEqual false

        set.join(1, 2) shouldEqual true
        set.join(2, 3) shouldEqual true
        set.join(1, 3) shouldEqual false

        set.areConnected(1, 2) shouldEqual true
        set.areConnected(1, 3) shouldEqual true
    }
}

class DisjointSet<T>() {
    private class Node<T>(val value: T) {
        var parent: Node<T> = this
        var size: Int = 1
    }

    private val nodes = HashMap<T, Node<T>>()

    constructor(iterable: Iterable<T>) : this() {
        addAll(iterable)
    }

    fun add(value: T) {
        nodes[value] = Node(value)
    }

    fun addAll(iterable: Iterable<T>) {
        iterable.forEach { add(it) }
    }

    fun areConnected(value1: T, value2: T): Boolean = find(value1) == find(value2)

    fun find(value: T): T = findNode(nodes[value]!!).value

    private tailrec fun findNode(node: Node<T>): Node<T> {
        return if (node.parent == node) node
        else findNode(node.parent)
    }

    fun join(value1: T, value2: T): Boolean {
        var root1 = findNode(nodes[value1]!!)
        var root2 = findNode(nodes[value2]!!)
        if (root1 == root2) return false

        if (root1.size < root2.size) {
            val tmp = root1
            root1 = root2
            root2 = tmp
        }
        root1.size = root1.size + root2.size
        root2.parent = root1
        return true
    }
}

private fun <T> Graph<T>.kruskalMST(): Graph<T> {
    val tree = Graph<T>()
    val set = DisjointSet(vertices)

    val queue = PriorityQueue<Edge<T>>(comparingInt { it.weight!! })
    queue.addAll(edgesByVertex.values.flatten())

    while (queue.isNotEmpty()) {
        val (from, to, weight) = queue.remove()
        if (set.join(from, to)) {
            tree.addEdge(from, to, weight)
        }
    }
    return tree
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
