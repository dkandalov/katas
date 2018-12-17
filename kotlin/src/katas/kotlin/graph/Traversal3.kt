package katas.kotlin.graph

import katas.kotlin.graph.Graph.Node
import kotlincommon.join
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*

class Traversal3 {
    @Test fun `depth-first traversal`() {
        "[a]".toGraph().dft("a").join("-") shouldEqual "a"
        "[a-b]".toGraph().dft("a").join("-") shouldEqual "a-b"

        "[a-b, b-c]".toGraph().let {
            it.dft("a").join("-") shouldEqual "a-b-c"
            it.dft("b").join("-") shouldEqual "b-a-c"
            it.dft("c").join("-") shouldEqual "c-b-a"
        }

        // a──b1──c
        // └──b2──┘
        "[a-b1, a-b2, b1-c, b2-c]".toGraph().let {
            it.dft("a").join("-") shouldEqual "a-b1-c-b2"
            it.dft("b1").join("-") shouldEqual "b1-a-b2-c"
            it.dft("b2").join("-") shouldEqual "b2-a-b1-c"
            it.dft("c").join("-") shouldEqual "c-b1-a-b2"
        }
    }

    @Test fun `breadth-first traversal`() {
        "[a]".toGraph().bft("a").join("-") shouldEqual "a"
        "[a-b]".toGraph().bft("a").join("-") shouldEqual "a-b"

        "[a-b, b-c]".toGraph().let {
            it.bft("a").join("-") shouldEqual "a-b-c"
            it.bft("b").join("-") shouldEqual "b-a-c"
            it.bft("c").join("-") shouldEqual "c-b-a"
        }

        // a──b1──c
        // └──b2──┘
        "[a-b1, a-b2, b1-c, b2-c]".toGraph().let {
            it.bft("a").join("-") shouldEqual "a-b1-b2-c"
            it.bft("b1").join("-") shouldEqual "b1-a-c-b2"
            it.bft("b2").join("-") shouldEqual "b2-a-c-b1"
            it.bft("c").join("-") shouldEqual "c-b1-b2-a"
        }
    }

    private fun <T, U> Graph<T, U>.bft(value: T) = node(value).bft()

    private fun <T, U> Node<T, U>.bft(): List<T> {
        val result = LinkedHashSet<T>()
        val queue = LinkedList<Node<T, U>>()
        queue.add(this)
        while (queue.isNotEmpty()) {
            val node = queue.removeFirst()
            if (!result.contains(node.value)) {
                result.add(node.value)
                queue.addAll(node.neighbors())
            }
        }
        return result.toList()
    }

    private fun <T, U> Graph<T, U>.dft(value: T) = node(value).dft()

    private fun <T, U> Node<T, U>.dft(visited: HashSet<Node<T, U>> = HashSet()): List<T> {
        val added = visited.add(this)
        return if (!added) emptyList()
        else listOf(this.value) + this.neighbors().flatMap { it.dft(visited) }
    }
}
