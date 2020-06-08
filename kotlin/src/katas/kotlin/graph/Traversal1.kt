package katas.kotlin.graph

import katas.kotlin.graph.Graph.Node
import nonstdlib.join
import datsok.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.LinkedHashSet

class Traversal1 {
    @Test fun `depth-first traversal`() {
        "[a]".toGraph().node("a").dft().join("-") shouldEqual "a"

        "[a-b1, a-b2, b1-c, b2-c]".toGraph().apply {
            node("a").dft().join("-") shouldEqual "a-b1-c-b2"
            node("b1").dft().join("-") shouldEqual "b1-a-b2-c"
            node("b2").dft().join("-") shouldEqual "b2-a-b1-c"
            node("c").dft().join("-") shouldEqual "c-b1-a-b2"
        }
    }

    @Test fun `bread-first traversal`() {
        "[a]".toGraph().node("a").bft().join("-") shouldEqual "a"

        "[a-b1, a-b2, b1-c, b2-c]".toGraph().apply {
            node("a").bft().join("-") shouldEqual "a-b1-b2-c"
            node("b1").bft().join("-") shouldEqual "b1-a-c-b2"
            node("b2").bft().join("-") shouldEqual "b2-a-c-b1"
            node("c").bft().join("-") shouldEqual "c-b1-b2-a"
        }
    }

    private fun <T, U> Node<T, U>.bft(): List<T> {
        val result = LinkedHashSet<T>()
        val queue = LinkedList(listOf(this))
        while (queue.isNotEmpty()) {
            val node = queue.removeFirst()
            val wasAdded = result.add(node.value)
            if (wasAdded) queue.addAll(node.neighbors())
        }
        return result.toList()
    }

    private fun <T, U> Node<T, U>.dft(visitedNodes: MutableSet<Node<T, U>> = HashSet()): List<T> {
        return if (visitedNodes.contains(this)) emptyList()
        else listOf(value) + neighbors().flatMap {
            visitedNodes.add(this)
            it.dft(visitedNodes)
        }
    }
}