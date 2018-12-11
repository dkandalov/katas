package katas.kotlin.hackerrank.shortest_path

import guru.nidi.graphviz.attribute.Label
import guru.nidi.graphviz.attribute.RankDir
import guru.nidi.graphviz.engine.*
import guru.nidi.graphviz.model.Factory.mutGraph
import guru.nidi.graphviz.model.Factory.mutNode
import guru.nidi.graphviz.model.Link
import katas.kotlin.hackerrank.OutputRecorder
import katas.kotlin.hackerrank.toReadLineFunction
import katas.kotlin.shouldEqual
import kotlincommon.printed
import org.junit.Test
import java.io.File
import java.util.*
import kotlin.collections.LinkedHashSet

/**
 * See https://www.hackerrank.com/challenges/dijkstrashortreach
 */
fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val i = generateSequence { scanner.nextLine() }.iterator()
    main({ i.next() })
}

private fun main(readLine: () -> String, writeLine: (Any?) -> Unit = { println(it) }) {
    val numberOfTests = readLine().toInt()
    (1..numberOfTests).map {
        val (nodeCount, edgeCount) = readLine().split(" ").let { Pair(it[0].toInt(), it[1].toInt()) }

        val graph = Graph(size = nodeCount)
        for (j in 0 until edgeCount) {
            val edge = readLine().split(" ").map { it.trim().toInt() }.toTypedArray()
            graph.addEdge(edge[0], edge[1], edge[2])
        }
        val startNode = readLine().trim().toInt()

        val result = graph.shortestPaths(startNode).minDist.toMutableList()
        result.removeAt(startNode)
        result.removeAt(0)
        writeLine(result.joinToString(" "))
    }
}

private data class Edge(val from: Int, val to: Int, val weight: Int)

private data class Graph(val size: Int, val edges: LinkedHashSet<Edge> = LinkedHashSet()) {
    val nodes: Set<Int> get() = IntRange(1, size).toSet()

    private val adjacency = LinkedHashMap<Pair<Int, Int>, Edge>()
    private val neighbours = LinkedHashMap<Int, LinkedHashSet<Int>>()

    fun addEdge(from: Int, to: Int, weight: Int) {
        if (from > to) return addEdge(to, from, weight)

        val existingEdge = adjacency[Pair(from, to)]
        if (existingEdge != null) {
            if (existingEdge.weight < weight) return
            else adjacency.remove(Pair(from, to))
        }

        val edge = Edge(from, to, weight)
        edges.add(edge)
        adjacency[Pair(from, to)] = edge
        neighbours.getOrPut(from, { LinkedHashSet() }).add(to)
        neighbours.getOrPut(to, { LinkedHashSet() }).add(from)
    }

    fun neighboursOf(node: Int): Set<Int> = neighbours[node] ?: emptySet()

    fun findEdge(from: Int, to: Int): Edge? =
        if (from > to) adjacency[Pair(to, from)]
        else adjacency[Pair(from, to)]
}

private data class Paths(val prevNode: List<Int>, val minDist: List<Int>)

private fun Graph.shortestPaths(fromNode: Int): Paths {
    val prev = Array(size + 1, { -1 }).toMutableList()
    val minDist = Array(size + 1, { Int.MAX_VALUE }).toMutableList()
    minDist[fromNode] = 0

    val queue = BinaryHeap<Int>(Comparator { node1, node2 -> minDist[node1].compareTo(minDist[node2]) })
    queue.addAll(nodes)

    while (queue.isNotEmpty()) {
        val node = queue.remove()

        val isUnreachableNode = minDist[node] == Int.MAX_VALUE
        if (isUnreachableNode) continue

        neighboursOf(node)
            .filter { queue.contains(it) }
            .forEach { neighbour ->
                val distance = minDist[node] + findEdge(node, neighbour)!!.weight
                if (distance < minDist[neighbour]) {
                    minDist[neighbour] = distance
                    prev[neighbour] = node
                    queue.updatePriorityOf(neighbour)
                }
            }
    }
    minDist.indices.forEach {
        if (minDist[it] == Int.MAX_VALUE) minDist[it] = -1
    }
    return Paths(prev, minDist)
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
    private val basePath = "src/katas/kotlin/hackerrank/shortest_path"

    @Test fun `basic example`() {
        Graph(size = 5).apply {
            addEdge(1, 2, weight = 5)
            addEdge(2, 3, weight = 6)
            addEdge(3, 4, weight = 2)
            addEdge(1, 3, weight = 15)

            val minDist = shortestPaths(fromNode = 1).minDist.printed()
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

            val minDist = shortestPaths(fromNode = 1).minDist.printed()
            minDist.drop(1) shouldEqual listOf(0, 24, 3, 15)

//            renderAsGraphViz().toFile(File("$basePath/sample-test-case-0.png"))
        }
    }

    @Test fun `sample tests case 1`() {
        Graph(size = 5).apply {
            addEdge(1, 2, weight = 10)
            addEdge(1, 3, weight = 6)
            addEdge(2, 4, weight = 8)

            val minDist = shortestPaths(fromNode = 2).minDist.printed()
            minDist.drop(1) shouldEqual listOf(10, 0, 16, 8, -1)

//            renderAsGraphViz().toFile(File("$basePath/sample-test-case-1.png"))
        }
    }

    @Test fun `test cases 2`() {
        val readLine = File("$basePath/test-cases-2.txt").readText().toReadLineFunction()
        val outputRecorder = OutputRecorder()

        main(readLine, outputRecorder)

        outputRecorder.text.trim() shouldEqual """
            20 25 25 68 86 39 22 70 36 53 91 35 88 27 30 43 54 74 41
            9 8 8 8 12 7 15 8 4 1 12 9 7 10 4 10 10 4 1 7 12 7 11 12 15 10 5 11 6 7 9 11 9 7 7 14 5 13 6 8 10 7 4 9 3 5 5 9 13 1 8 11 4 9 6 7 7 8 11 6 10 7 8 9 13 9 12 8 3 5 7 15 6 10 11 5 11
            154 90 186 190 178 114 123 -1 -1 123 -1 104 -1 -1 -1 207 134 123 98 155 -1 198 68 90 170 135 -1 103 145 -1 54 111 163 173 115 87 159 75 -1 94 102 -1 76 67 167 138 216 -1 172 102 212 163 103 112 -1 182 49 145 92 -1 -1 194 -1 182 -1 201 96 -1 85 121 108 161 130 100 120 -1 -1 118 215 92 156 162 163 168 71 110 -1 -1 190 217 100 105 178
            13 30 17 33 16 9 31 34 14 20 21 19 24 34 27 42 15 16 19 23 18 21 11 21 28 15 15 45 18 26 17 20 16 28 27 16 22 21 18 21 34 14 26 27 11 23 17 24 27 22 19 18 21 17 17 22 14 20 12 27 21 10 42 10 25 19 22
            3 6 8 11 7 12 10 18 4 8 3 6 12 1 2 10 1 8 5 6 9 9 8 17 11 12 8
            3 4 5 3 4 5 5 4 4 7 6 4 1 4 5 5 5 4 5 6 5 6 4 5 3 5 5 6 2 6 3 3 6 5 3 6 3 2 6 4 1 6 3 4 5 6 7 7 3 6 3 5 3 5 4 7 4 4 6 4 5 5 5 4 2 2 3 6 4 6 4 4 5 4 6 3 5 5 4 4 4 2 1 3 3 3 2
        """.trimIndent()
    }
}
