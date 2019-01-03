package katas.kotlin.hackerrank

import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.HashSet


class RoadsAndLibrariesTests {
    fun main(args: Array<String>) {
        val scan = Scanner(System.`in`)
        val q = scan.nextLine().trim().toInt()

        for (qItr in 1..q) {
            val nmC_libC_road = scan.nextLine().split(" ")
            val n = nmC_libC_road[0].trim().toInt()
            val m = nmC_libC_road[1].trim().toInt()
            val c_lib = nmC_libC_road[2].trim().toInt()
            val c_road = nmC_libC_road[3].trim().toInt()
            val cities = Array(m, { Array(2, { 0 }) })
            for (i in 0 until m) {
                cities[i] = scan.nextLine().split(" ").map { it.trim().toInt() }.toTypedArray()
            }

            println(roadsAndLibraries(n, c_lib, c_road, cities))
        }
    }

    private fun roadsAndLibraries(citiesCount: Int, libraryCost: Int, roadCost: Int, cityLinks: Array<Array<Int>>): Long {
        val graph = Graph<Int>()
        (1..citiesCount).forEach { graph.addVertex(it) }
        cityLinks.forEach { graph.addEdge(it[0], it[1]) }
        return findMinCost(graph, libraryCost, roadCost)
    }

    private fun findMinCost(graph: Graph<Int>, libraryCost: Int, roadCost: Int): Long {
        return graph.components().sumByLong { component ->
            val allLibsCost = component.vertices.size * libraryCost.toLong()
            val allRoadsCost = component.minSpanningTree().size * roadCost + libraryCost.toLong()
            minOf(allLibsCost, allRoadsCost)
        }
    }

    private data class Edge<T>(var from: T, var to: T, var weight: Int? = null) {
        override fun toString(): String {
            val weightString = if (weight != null) ", $weight" else ""
            return "Edge($from->$to$weightString)"
        }
    }

    private data class Graph<T>(val edgesByVertex: MutableMap<T, MutableList<Edge<T>>> = HashMap()) {
        val vertices: Set<T> get() = edgesByVertex.keys

        fun addEdge(from: T, to: T) {
            edgesByVertex.getOrPut(from, { ArrayList() }).add(Edge(from, to))
            edgesByVertex.getOrPut(to, { ArrayList() }).add(Edge(to, from))
        }

        fun addVertex(vertex: T) {
            edgesByVertex.getOrPut(vertex, { ArrayList() })
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

    private fun <T> Graph<T>.bfs(fromVertex: T = vertices.first()): List<T> {
        val result = ArrayList<T>()
        val wasQueued = HashSet<T>().apply { add(fromVertex) }
        val queue = LinkedList<T>().apply { add(fromVertex) }

        while (queue.isNotEmpty()) {
            val vertex = queue.removeFirst()
            result.add(vertex)
            edgesByVertex[vertex]?.map { it.to }
                ?.forEach {
                    val justAdded = wasQueued.add(it)
                    if (justAdded) queue.add(it)
                }
        }
        return result
    }

    private fun <T> Graph<T>.components(): List<Graph<T>> {
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

    private inline fun <T> Iterable<T>.sumByLong(selector: (T) -> Long): Long {
        var sum = 0L
        for (element in this) {
            sum += selector(element)
        }
        return sum
    }

    @Test fun `minimum spanning trees`() {
        Graph.readInts("1-2,2-3,3-1").minSpanningTree() shouldEqual listOf(Edge(1, 2), Edge(1, 3))
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
}