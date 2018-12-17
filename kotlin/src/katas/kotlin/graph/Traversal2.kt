package katas.kotlin.graph

import kotlincommon.join
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.LinkedHashSet

class Traversal2 {
    
    @Test fun `depth-first traversal`() {
        "[a]".toGraph().nodes["a"]!!.dft().join("-") shouldEqual "a"

        "[a-b1, a-b2, b1-c, b2-c]".toGraph().let { graph ->
            graph.nodes["a"]!!.dft().join("-") shouldEqual "a-b1-c-b2"
        }
    }

    @Test fun `breadth-first traversal`() {
        "[a-b1, a-b2, b1-c, b2-c]".toGraph().let { graph ->
            graph.nodes["a"]!!.bft().join("-") shouldEqual "a-b1-b2-c"
        }
    }

    private fun <T, U> Graph.Node<T, U>.dft(visited: MutableSet<T> = HashSet()): List<T> {
        val wasAdded = visited.add(value)
        return if (wasAdded) listOf(value) + neighbors().flatMap { it.dft(visited) }
        else emptyList()
    }

    private fun <T, U> Graph.Node<T, U>.bft(): List<T> {
        val result = LinkedHashSet<T>()
        val queue = LinkedList(listOf(this))
        while (queue.isNotEmpty()) {
            val node = queue.removeFirst()
            val wasAdded = result.add(node.value)
            if (wasAdded) queue.addAll(node.neighbors())
        }
        return result.toList()
    }
}
