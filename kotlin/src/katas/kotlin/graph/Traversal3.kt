package katas.kotlin.graph

import katas.kotlin.graph.Graph.Node
import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test

class Traversal3 {
    @Test fun `depth-first traversal`() {
        "[a]".toGraph().dft("a").join("-") shouldEqual "a"
        "[a-b]".toGraph().dft("a").join("-") shouldEqual "a-b"

        "[a-b, b-c]".let {
            it.toGraph().dft("a").join("-") shouldEqual "a-b-c"
            it.toGraph().dft("b").join("-") shouldEqual "b-a-c"
            it.toGraph().dft("c").join("-") shouldEqual "c-b-a"
        }

        "[a-b1, a-b2, b1-c, b2-c]".let {
            it.toGraph().dft("a").join("-") shouldEqual "a-b1-c-b2"
            it.toGraph().dft("b1").join("-") shouldEqual "b1-a-b2-c"
            it.toGraph().dft("b2").join("-") shouldEqual "b2-a-b1-c"
            it.toGraph().dft("c").join("-") shouldEqual "c-b1-a-b2"
        }
    }

    private fun <T, U> Graph<T, U>.dft(nodeValue: T) = node(nodeValue).dft(nodeValue)

    private fun <T, U> Node<T, U>.dft(value: T, visited: HashSet<Node<T, U>> = HashSet()): List<T> {
        val added = visited.add(this)
        return if (!added) emptyList()
        else listOf(this.value) + this.neighbors().flatMap { it.dft(value, visited) }
    }
}
