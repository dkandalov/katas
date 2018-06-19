package katas.kotlin.graph

import katas.kotlin.graph.Graph.Node
import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test
import java.util.*
import kotlin.collections.LinkedHashSet

class Traversal4 {
    @Test fun `depth-first traversal`() {
        "[a]".toGraph().let {
            it.dft("a") shouldEqual "a"
            it.dft("x") shouldEqual ""
        }
        "[a-b]".toGraph().let {
            it.dft("a") shouldEqual "a-b"
            it.dft("b") shouldEqual "b-a"
            it.dft("x") shouldEqual ""
        }
        "[a-b, b-c]".toGraph().let {
            it.dft("a") shouldEqual "a-b-c"
            it.dft("b") shouldEqual "b-a-c"
            it.dft("c") shouldEqual "c-b-a"
            it.dft("x") shouldEqual ""
        }
        // a──b1──c
        // └──b2──┘
        "[a-b1, a-b2, b1-c, b2-c]".toGraph().let {
            it.dft("a") shouldEqual "a-b1-c-b2"
            it.dft("b1") shouldEqual "b1-a-b2-c"
            it.dft("b2") shouldEqual "b2-a-b1-c"
            it.dft("c") shouldEqual "c-b1-a-b2"
        }
    }

    @Test fun `breadth-first traversal`() {
        "[a]".toGraph().let {
            it.bft("a") shouldEqual "a"
            it.bft("x") shouldEqual ""
        }
        "[a-b]".toGraph().let {
            it.bft("a") shouldEqual "a-b"
            it.bft("b") shouldEqual "b-a"
            it.bft("x") shouldEqual ""
        }
        "[a-b, b-c]".toGraph().let {
            it.bft("a") shouldEqual "a-b-c"
            it.bft("b") shouldEqual "b-a-c"
            it.bft("c") shouldEqual "c-b-a"
            it.bft("x") shouldEqual ""
        }
        // a──b1──c
        // └──b2──┘
        "[a-b1, a-b2, b1-c, b2-c]".toGraph().let {
            it.bft("a") shouldEqual "a-b1-b2-c"
            it.bft("b1") shouldEqual "b1-a-c-b2"
            it.bft("b2") shouldEqual "b2-a-c-b1"
            it.bft("c") shouldEqual "c-b1-b2-a"
        }
    }
}

private data class Result<T>(val traversal: LinkedHashSet<T>) {
    operator fun plus(value: T) = Result(LinkedHashSet(traversal + value))

    fun contains(value: T) = traversal.contains(value)

    companion object {
        fun <T> empty(): Result<T> = Result(LinkedHashSet())
    }
}

private fun <Value, Label> Graph<Value, Label>.bft(value: Value, separator: String = "-"): String {
    val node = nodes[value] ?: return ""
    return node.bft().traversal.join(separator)
}

private fun <Value, Label> Node<Value, Label>.bft(): Result<Value> {
    var result = Result.empty<Value>()
    val queue = LinkedList<Node<Value, Label>>()
    queue.add(this)
    while (queue.isNotEmpty()) {
        val node = queue.removeFirst()
        if (!result.contains(node.value)) {
            result += node.value
            queue.addAll(node.neighbors())
        }
    }
    return result
}

private fun <Value, Label> Graph<Value, Label>.dft(value: Value, separator: String = "-"): String {
    val node = nodes[value] ?: return ""
    return node.dft(Result.empty()).traversal.join(separator)
}

private fun <Value, Label> Node<Value, Label>.dft(result: Result<Value>): Result<Value> =
    if (result.contains(value)) result
    else neighbors().fold(result + value) { r, node -> node.dft(r) }
