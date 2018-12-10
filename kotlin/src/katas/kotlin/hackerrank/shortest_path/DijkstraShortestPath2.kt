package katas.kotlin.hackerrank.shortest_path

import guru.nidi.graphviz.attribute.Label
import guru.nidi.graphviz.attribute.RankDir
import guru.nidi.graphviz.engine.Format
import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.engine.Renderer
import guru.nidi.graphviz.model.Factory.mutGraph
import guru.nidi.graphviz.model.Factory.mutNode
import guru.nidi.graphviz.model.Link
import katas.kotlin.shouldEqual
import kotlincommon.printed
import org.junit.Test
import java.io.File
import java.util.*
import kotlin.collections.LinkedHashSet

// See https://www.hackerrank.com/challenges/dijkstrashortreach
fun shortestReach(numberOfNodes: Int, edges: Array<Array<Int>>, startNode: Int): Array<Int> {
    val graph = Graph(size = numberOfNodes)
    edges.forEach { (from, to, weight) ->
        graph.addEdge(from, to, weight)
    }
    val result = graph.shortestPaths(startNode).minDist.toMutableList()
    result.removeAt(startNode)
    result.removeAt(0)
    return result.toTypedArray()
}

private data class Edge(val from: Int, val to: Int, val weight: Int)

private data class Graph(val size: Int, val edges: LinkedHashSet<Edge> = LinkedHashSet()) {
    val nodes: Set<Int> get() = IntRange(1, size).toSet()

    private val adjacency = LinkedHashMap<Int, LinkedHashSet<Edge>>()
    private val neighbours = LinkedHashMap<Int, LinkedHashSet<Int>>()

    fun addEdge(from: Int, to: Int, weight: Int) {
        val edge = Edge(from, to, weight)
        edges.add(edge)

        adjacency.getOrPut(from, { LinkedHashSet() }).add(edge)
        adjacency.getOrPut(to, { LinkedHashSet() }).add(edge)

        neighbours.getOrPut(from, { LinkedHashSet() }).add(to)
        neighbours.getOrPut(to, { LinkedHashSet() }).add(from)
    }

    fun neighboursOf(node: Int): Set<Int> = neighbours[node] ?: emptySet()

    fun findEdge(node1: Int, node2: Int): Edge? {
        return adjacency[node1]!!.find { (from, to, _) ->
            (from == node1 && to == node2) ||
                (from == node2 && to == node1)
        }
    }
}

private data class Paths(val minDist: List<Int>, val prevNode: List<Int>)

private fun Graph.shortestPaths(fromNode: Int): Paths {
    val prev = Array(size + 1, { -1 }).toMutableList()
    val minDist = Array(size + 1, { Int.MAX_VALUE }).toMutableList()
    minDist[fromNode] = 0

    val queue = BinaryHeap<Int>(Comparator { node1, node2 -> minDist[node1].compareTo(minDist[node2]) })
    queue.addAll(nodes)

    while (queue.isNotEmpty()) {
        val current = queue.removeMin()

        neighboursOf(current)
            .filter { queue.contains(it) }
            .forEach { neighbour ->
                val distance = minDist[current] + findEdge(current, neighbour)!!.weight
                if (distance < minDist[neighbour] || minDist[neighbour] == Int.MAX_VALUE) {
                    minDist[neighbour] = distance
                    prev[neighbour] = current
                    queue.updatePriorityOf(neighbour)
                }
            }
    }
    minDist.indices.forEach {
        if (minDist[it] == Int.MAX_VALUE) minDist[it] = -1
    }
    return Paths(minDist, prev)
}

private fun Graph.renderAsGraphViz(graphName: String = "graph", height: Int = 500): Renderer {
    val graphViz = mutGraph(graphName).setDirected(false)
    graphViz.graphAttrs().add(RankDir.LEFT_TO_RIGHT)

    val graphVizNodeById = nodes
        .map { it.toString() }
        .associate { Pair(it, mutNode(it)) }

    graphVizNodeById.values.forEach {
        graphViz.add(it)
    }

    edges.forEach { (from, to, weight) ->
        val fromNode = graphVizNodeById[from.toString()]!!
        val toNode = graphVizNodeById[to.toString()]!!
        val label = Label.of(weight.toString())
        fromNode.addLink(Link.to(toNode).with(label))
    }
    return Graphviz.fromGraph(graphViz).height(height).render(Format.PNG)
}

class DijkstraShortestPath2Tests {
    @Test fun `basic example`() {
        Graph(size = 5).apply {
            addEdge(1, 2, weight = 5)
            addEdge(2, 3, weight = 6)
            addEdge(3, 4, weight = 2)
            addEdge(1, 3, weight = 15)

            val (minDist, _) = shortestPaths(fromNode = 1).printed()
            minDist.drop(1) shouldEqual listOf(0, 5, 11, 13, -1)

            // renderAsGraphViz().toFile(File("src/katas/kotlin/hackerrank/shortest_path/basic-example.png"))
        }
    }

    @Test fun `sample tests case 0`() {
        Graph(size = 4).apply {
            addEdge(1, 2, weight = 24)
            addEdge(1, 4, weight = 20)
            addEdge(3, 1, weight = 3)
            addEdge(4, 3, weight = 12)

            val (minDist, _) = shortestPaths(fromNode = 1).printed()
            minDist.drop(1) shouldEqual listOf(0, 24, 3, 15)

             renderAsGraphViz().toFile(File("src/katas/kotlin/hackerrank/shortest_path/sample-test-case-0.png"))
        }
    }

    @Test fun `sample tests case 1`() {
        Graph(size = 5).apply {
            addEdge(1, 2, weight = 10)
            addEdge(1, 3, weight = 6)
            addEdge(2, 4, weight = 8)

            val (minDist, _) = shortestPaths(fromNode = 2).printed()
            minDist.drop(1) shouldEqual listOf(10, 0, 16, 8, -1)

            renderAsGraphViz().toFile(File("src/katas/kotlin/hackerrank/shortest_path/sample-test-case-1.png"))
        }
    }
}


class FibonacciHeap<E : Comparable<E>> {
    private data class Node<E>(val value: E) {
        lateinit var next: Node<E>
        lateinit var prev: Node<E>

        fun leftInsert(node: Node<E>): Node<E> {
            prev.next = node
            node.prev = prev
            node.next = this
            prev = node
            return node
        }
    }

    private var minNode: Node<E>? = null

    fun add(value: E) {
        if (minNode == null) {
            minNode = Node(value).apply {
                next = this
                prev = this
            }
        } else {
            val node = minNode!!.leftInsert(Node(value))
            if (node.value < minNode!!.value) minNode = node
        }
    }

    fun minValue() = minNode?.value

    fun removeMin(): E? {
        if (minNode == null) return null
        TODO()
    }

    override fun toString(): String {
        if (minNode == null) return "[]"
        val rootNodes = mutableListOf(minNode)
        while (rootNodes.last()!!.next != rootNodes.first()) {
            rootNodes.add(rootNodes.last()?.next)
        }
        return rootNodes.requireNoNulls().joinToString { it.value.toString() }
    }
}

class FibonacciHeapTests {
    @Test fun `construct heap`() {
        val heap = FibonacciHeap<Int>()
        heap.add(3)
        heap.add(1)
        heap.add(2)
        heap.printed()
        heap.minValue() shouldEqual 1
    }
}