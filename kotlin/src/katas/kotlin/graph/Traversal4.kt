package katas.kotlin.graph

import katas.kotlin.graph.Graph.Node
import nonstdlib.join
import datsok.shouldEqual
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

    private data class Result<T>(val path: LinkedHashSet<T>) {
        operator fun plus(value: T) = Result(LinkedHashSet(path + value))

        fun contains(value: T) = path.contains(value)

        companion object {
            fun <T> empty(): Result<T> = Result(LinkedHashSet())
        }
    }

    private interface Container<T> {
        fun add(value: T)
        fun add(values: Collection<T>)
        fun remove(): T
        fun isNotEmpty(): Boolean

        companion object {
            fun <T> queue() = object: Container<T> {
                val list = LinkedList<T>()
                override fun add(value: T) = list.addFirst(value)
                override fun add(values: Collection<T>) { list.addAll(values) }
                override fun remove() = list.removeFirst()
                override fun isNotEmpty() = list.isNotEmpty()
            }
            fun <T> stack() = object: Container<T> {
                val list = LinkedList<T>()
                override fun add(value: T) = list.addLast(value)
                override fun add(values: Collection<T>) { list.addAll(values.reversed()) }
                override fun remove() = list.removeLast()
                override fun isNotEmpty() = list.isNotEmpty()
            }
        }
    }


    private fun <Value, Label> Graph<Value, Label>.bft(value: Value, separator: String = "-"): String {
        val node = nodes[value] ?: return ""
        return node.traverse(Container.queue()).path.join(separator)
    }

    private fun <Value, Label> Graph<Value, Label>.dft(value: Value, separator: String = "-"): String {
        val node = nodes[value] ?: return ""
        return node.traverse(Container.stack()).path.join(separator)
    }

    private fun <Value, Label> Node<Value, Label>.traverse(container: Container<Node<Value, Label>>): Result<Value> {
        var result = Result.empty<Value>()
        container.add(this)
        while (container.isNotEmpty()) {
            val node = container.remove()
            if (!result.contains(node.value)) {
                result += node.value
                container.add(node.neighbors())
            }
        }
        return result
    }
}