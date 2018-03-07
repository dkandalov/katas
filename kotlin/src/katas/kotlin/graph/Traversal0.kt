package katas.kotlin.graph

import katas.kotlin.graph.Graph.Node
import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test
import java.util.*

class Traversal0 {
    @Test fun `depth-first traversal`() {
        "[a-b, b-c, a-d]".toGraph().apply {
            node("a").dft().join("") shouldEqual "abcd"
            node("b").dft().join("") shouldEqual "badc"
            node("c").dft().join("") shouldEqual "cbad"
            node("d").dft().join("") shouldEqual "dabc"
        }
        "[a-b, b-c, a-d, d-c, c-e]".toGraph().apply {
            node("a").dft().join("") shouldEqual "abcde"
            node("e").dft().join("") shouldEqual "ecbad"
        }
    }

    @Test fun `breadth-first traversal`() {
        "[a-b, b-c, a-d]".toGraph().apply {
            node("a").bft().join("") shouldEqual "abdc"
            node("b").bft().join("") shouldEqual "bacd"
            node("c").bft().join("") shouldEqual "cbad"
            node("d").bft().join("") shouldEqual "dabc"
        }
        "[a-b, b-c, a-d, d-c, c-e]".toGraph().apply {
            node("a").bft().join("") shouldEqual "abdce"
            node("e").bft().join("") shouldEqual "ecbda"
        }
    }


    private fun <T, U> Node<T, U>.bft(): List<T> {
        val result = ArrayList<T>()
        val queue = LinkedList(listOf(this))
        while (queue.isNotEmpty()) {
            val node = queue.removeFirst()
            if (result.contains(node.value)) continue
            result.add(node.value)
            queue.addAll(node.neighbors())
        }
        return result
    }

    private fun <T, U> Node<T, U>.dft(visited: HashSet<Node<T, U>> = HashSet()): List<T> {
        return if (visited.contains(this)) emptyList()
        else {
            visited.add(this)
            listOf(value) + neighbors().flatMap { it.dft(visited) }
        }
    }
}
