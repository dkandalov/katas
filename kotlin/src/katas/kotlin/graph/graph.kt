package katas.kotlin.graph

import katas.kotlin.graph.Graph.TermForm
import katas.kotlin.graph.Graph.TermForm.Term
import java.util.*
import java.util.regex.Pattern

@Suppress("unused") // Copied from https://github.com/dkandalov/kotlin-99
class Graph<Value, Label>(nodes: Collection<Node<Value, Label>> = emptyList(), edges: Collection<Edge<Value, Label>> = emptyList()) {
    val nodes: MutableMap<Value, Node<Value, Label>> = nodes
        .map { Pair(it.value, it) }
        .toMap(LinkedHashMap()) // Use linked map to make operations on graph more deterministic.
    val edges: MutableList<Edge<Value, Label>> = edges.toMutableList()

    private fun addNode(value: Value): Node<Value, Label> {
        val node = Node<Value, Label>(value)
        nodes[value] = node
        return node
    }

    private fun addUndirectedEdge(n1: Value, n2: Value, label: Label?) {
        if (!nodes.contains(n1) || !nodes.contains(n2)) {
            throw IllegalStateException("Expected '$n1' and '$n2' nodes to exist in graph")
        }
        val edge = UndirectedEdge(nodes[n1]!!, nodes[n2]!!, label)
        if (edges.none { it.equivalentTo(edge) }) {
            edges.add(edge)
            nodes[n1]!!.edges.add(edge)
            nodes[n2]!!.edges.add(edge)
        }
    }

    private fun addDirectedEdge(source: Value, dest: Value, label: Label?) {
        val edge = DirectedEdge(nodes[source]!!, nodes[dest]!!, label)
        if (!edges.contains(edge)) {
            edges.add(edge)
            nodes[source]!!.edges.add(edge)
        }
    }

    override fun toString(): String {
        val standaloneNodes = nodes.values.filter { node -> edges.all { it.n1 != node && it.n2 != node } }
        val s = (edges.map { it.toString() } + standaloneNodes.map { it.toString() }).joinToString()
        return "[$s]"
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other?.javaClass != javaClass) return false
        other as Graph<*, *>
        return nodes == other.nodes && edges == other.edges
    }

    override fun hashCode() = 31 * nodes.hashCode() + edges.hashCode()

    fun equivalentTo(other: Graph<Value, Label>): Boolean {
        return nodes == other.nodes && edges.all { edge -> other.edges.any { it.equivalentTo(edge) } }
    }


    data class Node<T, U>(val value: T) {
        val edges: MutableList<Edge<T, U>> = ArrayList()
        fun neighbors(): List<Node<T, U>> = edges.map { edge -> edge.target(this)!! }
        override fun toString() = value.toString()
    }

    interface Edge<T, U> {
        val n1: Node<T, U>
        val n2: Node<T, U>
        val label: U?
        fun target(node: Node<T, U>): Node<T, U>?
        fun equivalentTo(other: Edge<T, U>) =
            (n1 == other.n1 && n2 == other.n2) || (n1 == other.n2 && n2 == other.n1)
    }

    data class UndirectedEdge<T, U>(override val n1: Node<T, U>, override val n2: Node<T, U>, override val label: U?): Edge<T, U> {
        override fun target(node: Node<T, U>) = if (n1 == node) n2 else if (n2 == node) n1 else null
        override fun toString() = n1.toString() + "-" + n2 + (if (label == null) "" else "/" + label.toString())
    }

    data class DirectedEdge<T, U>(override val n1: Node<T, U>, override val n2: Node<T, U>, override val label: U?): Edge<T, U> {
        override fun target(node: Node<T, U>) = if (n1 == node) n2 else null
        override fun toString() = n1.toString() + ">" + n2 + (if (label == null) "" else "/" + label.toString())
    }


    data class TermForm<out T, out U>(val nodes: Collection<T>, val edges: List<Term<T, U>>) {
        data class Term<out T, out U>(val n1: T, val n2: T, val label: U? = null) {
            override fun toString() = if (label == null) "Term($n1, $n2)" else "Term($n1, $n2, $label)"
        }
    }

    data class AdjacencyList<T, out U>(val entries: List<Entry<T, U>>) {
        constructor(vararg entries: Entry<T, U>): this(entries.asList())
        override fun toString() = "AdjacencyList(${entries.joinToString()})"

        data class Entry<out T, out U>(val node: T, val links: List<Link<T, U>> = emptyList<Nothing>()) {
            constructor(node: T, vararg links: Link<T, U>): this(node, links.asList())
            override fun toString() = "Entry($node, links[${links.joinToString()}])"
            companion object {
                fun <T> links(vararg linkValues: T): List<Link<T, Nothing>> = linkValues.map { Link(it, null) }
            }
        }

        data class Link<out T, out U>(val node: T, val label: U? = null) {
            override fun toString() = if (label == null) "$node" else "$node/$label"
        }
    }

    companion object {
        fun <T> terms(termForm: TermForm<T, Nothing>): Graph<T, Nothing> =
            createFromTerms(termForm) { graph, n1, n2, value -> graph.addUndirectedEdge(n1, n2, value) }

        fun <T> directedTerms(termForm: TermForm<T, Nothing>): Graph<T, Nothing> =
            createFromTerms(termForm) { graph, n1, n2, value -> graph.addDirectedEdge(n1, n2, value) }

        fun <T, U> labeledTerms(termForm: TermForm<T, U>): Graph<T, U> =
            createFromTerms(termForm) { graph, n1, n2, value -> graph.addUndirectedEdge(n1, n2, value) }

        fun <T, U> labeledDirectedTerms(termForm: TermForm<T, U>): Graph<T, U> =
            createFromTerms(termForm) { graph, n1, n2, value -> graph.addDirectedEdge(n1, n2, value) }

        fun <T> adjacent(adjacencyList: AdjacencyList<T, Nothing>): Graph<T, *> =
            fromAdjacencyList(adjacencyList) { graph, n1, n2, value ->
                graph.addUndirectedEdge(n1, n2, value)
            }

        fun <T> directedAdjacent(adjacencyList: AdjacencyList<T, Nothing>): Graph<T, *> =
            fromAdjacencyList(adjacencyList) { graph, n1, n2, value -> graph.addDirectedEdge(n1, n2, value) }

        fun <T, U> labeledAdjacent(adjacencyList: AdjacencyList<T, U>): Graph<T, U> =
            fromAdjacencyList(adjacencyList) { graph, n1, n2, value ->
                graph.addUndirectedEdge(n1, n2, value)
            }

        fun <T, U> labeledDirectedAdjacent(adjacencyList: AdjacencyList<T, U>): Graph<T, U> =
            fromAdjacencyList(adjacencyList) { graph, n1, n2, value ->
                graph.addDirectedEdge(n1, n2, value)
            }

        private fun <T, U> createFromTerms(termForm: TermForm<T, U>, addFunction: (Graph<T, U>, T, T, U?) -> Unit): Graph<T, U> {
            val graph = Graph<T, U>()
            termForm.nodes.forEach { graph.addNode(it) }
            termForm.edges.forEach { addFunction(graph, it.n1, it.n2, it.label) }
            return graph
        }

        private fun <T, U> fromAdjacencyList(adjacencyList: AdjacencyList<T, U>, addFunction: (Graph<T, U>, T, T, U?) -> Unit): Graph<T, U> {
            val graph = Graph<T, U>()
            adjacencyList.entries.forEach { graph.addNode(it.node) }
            adjacencyList.entries.forEach {
                val (node, links) = it
                links.forEach { addFunction(graph, node, it.node, it.label) }
            }
            return graph
        }
    }
}

private val graphTokenSeparators = Pattern.compile("[->/]")

fun String.toGraph(): Graph<String, Nothing> {
    if (!startsWith('[') || !endsWith(']')) {
        throw IllegalArgumentException("Expected string starting '[' and ending with ']' but it was '$this'")
    }
    val tokens = substring(1, length - 1).split(", ").map { it.split(graphTokenSeparators) }
    val nodes = tokens.flatMap { it }.toCollection(LinkedHashSet())
    val edges = tokens.filter { it.size == 2 }.map { Term<String, Nothing>(it[0], it[1]) }
    return if (contains("-")) {
        Graph.terms(TermForm(nodes, edges))
    } else {
        Graph.directedTerms(TermForm(nodes, edges))
    }
}

fun <T, U> Graph<T, U>.node(t: T): Graph.Node<T, U> = nodes[t]!!
