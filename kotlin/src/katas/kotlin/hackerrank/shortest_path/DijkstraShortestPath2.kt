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
import java.util.*
import kotlin.collections.LinkedHashSet


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

private fun Graph.shortestPaths(from: Int): Paths {
    val prev = Array(size + 1, { -1 }).toMutableList()
    val minDist = Array(size + 1, { Int.MAX_VALUE }).toMutableList()
    minDist[from] = 0

    val queue = PriorityQueue<Int>(Comparator.comparingInt({ node: Int -> minDist[node] }))
    queue.addAll(nodes)

    while (queue.isNotEmpty()) {
        val current = queue.remove()

        neighboursOf(current)
            .filter { queue.contains(it) }
            .forEach { neighbour ->
                val distance = minDist[current] + findEdge(current, neighbour)!!.weight
                if (distance < minDist[neighbour] || minDist[neighbour] == Int.MAX_VALUE) {
                    minDist[neighbour] = distance
                    prev[neighbour] = current
                }
            }

        val elements = queue.toList()
        queue.clear()
        queue.addAll(elements)
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

fun main(args: Array<String>) {
    Graph(size = 5).apply {
        addEdge(1, 2, weight = 5)
        addEdge(2, 3, weight = 6)
        addEdge(3, 4, weight = 2)
        addEdge(1, 3, weight = 15)

        val (minDist, _) = shortestPaths(1).printed()
        minDist.drop(2) shouldEqual listOf(5, 11, 13, -1)

        // renderAsGraphViz().toFile(File("src/katas/kotlin/hackerrank/shortest_path/basic-example.png"))
    }
    Graph(size = 4).apply {
        addEdge(1, 2, weight = 24)
        addEdge(1, 4, weight = 20)
        addEdge(3, 1, weight = 3)
        addEdge(4, 3, weight = 12)

        val (minDist, _) = shortestPaths(1).printed()
        minDist.drop(2) shouldEqual listOf(24, 3, 15)

        // renderAsGraphViz().toFile(File("src/katas/kotlin/hackerrank/shortest_path/another-example.png"))
    }
}