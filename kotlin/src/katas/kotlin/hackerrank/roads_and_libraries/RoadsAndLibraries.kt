package katas.kotlin.hackerrank.roads_and_libraries

import katas.kotlin.hackerrank.OutputRecorder
import katas.kotlin.hackerrank.toReadLineFunction
import nonstdlib.printed
import datsok.shouldEqual
import org.junit.Test
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.util.*
import kotlin.collections.HashMap


class RoadsAndLibrariesTests {
    /**
     * https://www.hackerrank.com/challenges/torque-and-development
     */
    fun main(args: Array<String>) {
        val reader = BufferedReader(InputStreamReader(System.`in`))
        main({ reader.readLine() })
    }

    private fun main(readLine: () -> String, writeLine: (Any?) -> Unit = { println(it) }) {
        val q = readLine().toInt()
        (1..1).forEach {
            val split = readLine().split(' ')
            val citiesCount = split[0].toInt().printed("citiesCount ")
            val cityLinksCount = split[1].toInt().printed("cityLinksCount ")
            val libraryCost = split[2].toInt()
            val roadCost = split[3].toInt()
            val cities = (0 until cityLinksCount).mapTo(ArrayList(cityLinksCount)) {
                readLine().split(' ').map { it.toInt() }
            }
            writeLine(roadsAndLibraries(citiesCount, libraryCost, roadCost, cities))
        }
    }

    private fun roadsAndLibraries(citiesCount: Int, libraryCost: Int, roadCost: Int, cityLinks: List<List<Int>>): Long {
        val graph = Graph<Int>()
        (1..citiesCount).forEach { graph.addVertex(it) }
        cityLinks.forEach { graph.addEdge(it[0], it[1]) }
        return findMinCost(graph, libraryCost, roadCost)
    }

    private fun findMinCost(graph: Graph<Int>, libraryCost: Int, roadCost: Int): Long {
        val components = graph.components()
        components.size.printed("componentsSize ")
        return components.sumByLong { component ->
            val minSpanningTreeEdgesSize = component.vertices.size - 1
            val allRoadsCost = minSpanningTreeEdgesSize * roadCost + libraryCost.toLong()
            val allLibsCost = component.vertices.size * libraryCost.toLong()
            minOf(allLibsCost, allRoadsCost)
        }
    }

    private fun <T> Graph<T>.minSpanningTree(): List<Edge<T>> {
        val result = ArrayList<Edge<T>>()

        val queuedVertices = HashSet<T>().apply { add(vertices.first()) }
        val queue = LinkedList<T>().apply { add(vertices.first()) }

        while (queue.isNotEmpty()) {
            val vertex = queue.removeFirst()
            edgesByVertex[vertex]?.forEach { edge ->
                if (queuedVertices.add(edge.to)) {
                    result.add(edge)
                    queue.add(edge.to)
                }
            }
        }
        return result
    }

    private data class Edge<T>(var from: T, var to: T, var weight: Int? = null) {
        override fun toString(): String {
            val weightString = if (weight != null) ", $weight" else ""
            return "Edge($from->$to$weightString)"
        }
    }

    private data class Graph<T>(val edgesByVertex: MutableMap<T, MutableList<Edge<T>>?> = HashMap()) {
        val vertices: Set<T> get() = edgesByVertex.keys

        fun addEdge(from: T, to: T) {
            edgesByVertex.getOrPut(from, { ArrayList() })!!.add(Edge(from, to))
            edgesByVertex.getOrPut(to, { ArrayList() })!!.add(Edge(to, from))
        }

        fun addVertex(vertex: T) {
            edgesByVertex[vertex] = null
        }

        companion object {
            fun readInts(s: String): Graph<Int> = read(s) { it.toInt() }

            fun <T> read(s: String, parse: (String) -> T): Graph<T> {
                val graph = Graph<T>()
                s.split(",").forEach { token ->
                    val split = token.split("-")
                    if (split.size == 2) {
                        graph.addEdge(from = parse(split[0]), to = parse(split[1]))
                    } else {
                        graph.addVertex(parse(split[0]))
                    }
                }
                return graph
            }
        }
    }

    private fun <T> Graph<T>.components(): List<Graph<T>> {
        val result = ArrayList<Graph<T>>()
        val graphByVertex = HashMap<T, Graph<T>>()
        vertices.forEach { vertex ->
            var graph = graphByVertex[vertex]
            if (graph == null) {
                graph = Graph()
                result.add(graph)
                graphByVertex[vertex] = graph
            }
            val neighbourEdges = edgesByVertex[vertex]
            graph.edgesByVertex[vertex] = neighbourEdges
            neighbourEdges?.forEach { (_, to) -> graphByVertex[to] = graph }
        }
        return result
    }

    private inline fun <T> Iterable<T>.sumByLong(selector: (T) -> Long): Long {
        var sum = 0L
        for (element in this) {
            sum += selector(element)
        }
        return sum
    }

    @Test fun `example from problem description`() {
        val graph = Graph.readInts("1-2,1-3,1-7,2-3,5-6,6-8")
        findMinCost(graph, libraryCost = 3, roadCost = 2) shouldEqual 16
    }

    @Test fun `sample test case 0`() {
        findMinCost(Graph.readInts("1-2,2-3,3-1"), libraryCost = 2, roadCost = 1) shouldEqual 4
        findMinCost(Graph.readInts("1-3,3-4,2-4,1-2,2-3,5-6"), libraryCost = 2, roadCost = 5) shouldEqual 12
    }

    @Test fun `sample test case 1`() {
        findMinCost(Graph.readInts("1-2,1-3,4-5,4-6"), libraryCost = 2, roadCost = 3) shouldEqual 12
    }

    @Test fun `sample test case 2`() {
        findMinCost(Graph.readInts("1-2,1-3,1-4,5"), libraryCost = 6, roadCost = 1) shouldEqual 15
    }

    @Test fun `test case 2`() {
        val readLine = File("src/katas/kotlin/hackerrank/roads_and_libraries/test-case2.txt").inputStream().toReadLineFunction()
        val outputRecorder = OutputRecorder()

        main(readLine, outputRecorder)

        outputRecorder.text.trim() shouldEqual """
            5649516
            5295483
            9261576
            3960530
            7629795
            40216260
            6701050
            40280315
            4614540
            12407190
        """.trimIndent()
    }

    @Test fun `test case 3`() {
        val readLine = File("src/katas/kotlin/hackerrank/roads_and_libraries/test-case3.txt").inputStream().toReadLineFunction()
        val outputRecorder = OutputRecorder()

        main(readLine, outputRecorder)

        outputRecorder.text.trim() shouldEqual """
            7850257285
            6785201034
            813348013
            4211840970
            8610471142
            7263742960
            4331105640
            1226092626
            7288635830
            8276704464
        """.trimIndent()
    }

    @Test fun `test case 4`() {
        val readLine = File("src/katas/kotlin/hackerrank/roads_and_libraries/test-case4.txt").inputStream().toReadLineFunction()
        val outputRecorder = OutputRecorder()

        main(readLine, outputRecorder)

        outputRecorder.text.trim() shouldEqual """
            9234981465
            5854508506
            7754252297
            8085193494
            9504556779
            8011172848
            9123393445
            7326423794
            8259748808
            8049633228
        """.trimIndent()
    }
}