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


    data class Node<Value, Label>(val value: Value) {
        val edges: MutableList<Edge<Value, Label>> = ArrayList()
        fun neighbors(): List<Node<Value, Label>> = edges.map { edge -> edge.target(this)!! }
        override fun toString() = value.toString()
    }

    interface Edge<Value, Label> {
        val n1: Node<Value, Label>
        val n2: Node<Value, Label>
        val label: Label?
        fun target(node: Node<Value, Label>): Node<Value, Label>?
        fun equivalentTo(other: Edge<Value, Label>) =
            (n1 == other.n1 && n2 == other.n2) || (n1 == other.n2 && n2 == other.n1)
    }

    data class UndirectedEdge<Value, Label>(override val n1: Node<Value, Label>, override val n2: Node<Value, Label>, override val label: Label?): Edge<Value, Label> {
        override fun target(node: Node<Value, Label>) = if (n1 == node) n2 else if (n2 == node) n1 else null
        override fun toString() = n1.toString() + "-" + n2 + (if (label == null) "" else "/" + label.toString())
    }

    data class DirectedEdge<Value, Label>(override val n1: Node<Value, Label>, override val n2: Node<Value, Label>, override val label: Label?): Edge<Value, Label> {
        override fun target(node: Node<Value, Label>) = if (n1 == node) n2 else null
        override fun toString() = n1.toString() + ">" + n2 + (if (label == null) "" else "/" + label.toString())
    }


    data class TermForm<out Value, out Label>(val nodes: Collection<Value>, val edges: List<Term<Value, Label>>) {
        data class Term<out Value, out Label>(val n1: Value, val n2: Value, val label: Label? = null) {
            override fun toString() = if (label == null) "Term($n1, $n2)" else "Term($n1, $n2, $label)"
        }
    }

    data class AdjacencyList<Value, out Label>(val entries: List<Entry<Value, Label>>) {
        constructor(vararg entries: Entry<Value, Label>): this(entries.asList())
        override fun toString() = "AdjacencyList(${entries.joinToString()})"

        data class Entry<out Value, out Label>(val node: Value, val links: List<Link<Value, Label>> = emptyList<Nothing>()) {
            constructor(node: Value, vararg links: Link<Value, Label>): this(node, links.asList())
            override fun toString() = "Entry($node, links[${links.joinToString()}])"
            companion object {
                fun <Value> links(vararg linkValues: Value): List<Link<Value, Nothing>> = linkValues.map { Link(it, null) }
            }
        }

        data class Link<out Value, out Label>(val node: Value, val label: Label? = null) {
            override fun toString() = if (label == null) "$node" else "$node/$label"
        }
    }

    companion object {
        fun <Value> terms(termForm: TermForm<Value, Nothing>): Graph<Value, Nothing> =
            createFromTerms(termForm) { graph, n1, n2, value -> graph.addUndirectedEdge(n1, n2, value) }

        fun <Value> directedTerms(termForm: TermForm<Value, Nothing>): Graph<Value, Nothing> =
            createFromTerms(termForm) { graph, n1, n2, value -> graph.addDirectedEdge(n1, n2, value) }

        fun <Value, Label> labeledTerms(termForm: TermForm<Value, Label>): Graph<Value, Label> =
            createFromTerms(termForm) { graph, n1, n2, value -> graph.addUndirectedEdge(n1, n2, value) }

        fun <Value, Label> labeledDirectedTerms(termForm: TermForm<Value, Label>): Graph<Value, Label> =
            createFromTerms(termForm) { graph, n1, n2, value -> graph.addDirectedEdge(n1, n2, value) }

        fun <Value> adjacent(adjacencyList: AdjacencyList<Value, Nothing>): Graph<Value, *> =
            fromAdjacencyList(adjacencyList) { graph, n1, n2, value ->
                graph.addUndirectedEdge(n1, n2, value)
            }

        fun <Value> directedAdjacent(adjacencyList: AdjacencyList<Value, Nothing>): Graph<Value, *> =
            fromAdjacencyList(adjacencyList) { graph, n1, n2, value -> graph.addDirectedEdge(n1, n2, value) }

        fun <Value, Label> labeledAdjacent(adjacencyList: AdjacencyList<Value, Label>): Graph<Value, Label> =
            fromAdjacencyList(adjacencyList) { graph, n1, n2, value ->
                graph.addUndirectedEdge(n1, n2, value)
            }

        fun <Value, Label> labeledDirectedAdjacent(adjacencyList: AdjacencyList<Value, Label>): Graph<Value, Label> =
            fromAdjacencyList(adjacencyList) { graph, n1, n2, value ->
                graph.addDirectedEdge(n1, n2, value)
            }

        private fun <Value, Label> createFromTerms(
            termForm: TermForm<Value, Label>,
            addFunction: (Graph<Value, Label>, Value, Value, Label?) -> Unit
        ): Graph<Value, Label> {
            val graph = Graph<Value, Label>()
            termForm.nodes.forEach { graph.addNode(it) }
            termForm.edges.forEach { addFunction(graph, it.n1, it.n2, it.label) }
            return graph
        }

        private fun <Value, Label> fromAdjacencyList(
            adjacencyList: AdjacencyList<Value, Label>,
            addFunction: (Graph<Value, Label>, Value, Value, Label?) -> Unit
        ): Graph<Value, Label> {
            val graph = Graph<Value, Label>()
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

fun <Value, Label> Graph<Value, Label>.node(t: Value): Graph.Node<Value, Label> = nodes[t]!!
