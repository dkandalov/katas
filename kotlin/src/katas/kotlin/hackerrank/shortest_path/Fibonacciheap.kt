package katas.kotlin.hackerrank.shortest_path

import nonstdlib.printed
import datsok.shouldEqual
import org.junit.Test


class FibonacciHeap<E : Comparable<E>> {
    private data class Node<E>(val value: E) {
        lateinit var next: Node<E>
        lateinit var prev: Node<E>

        fun leftInsert(node: Node<E>): Node<E> {
            prev.next = node
            node.prev = prev
            node.next = this
            prev = node
            return node
        }
    }

    private var minNode: Node<E>? = null

    fun add(value: E) {
        if (minNode == null) {
            minNode = Node(value).apply {
                next = this
                prev = this
            }
        } else {
            val node = minNode!!.leftInsert(Node(value))
            if (node.value < minNode!!.value) minNode = node
        }
    }

    fun minValue() = minNode?.value

    fun removeMin(): E? {
        if (minNode == null) return null
        TODO()
    }

    override fun toString(): String {
        if (minNode == null) return "[]"
        val rootNodes = mutableListOf(minNode)
        while (rootNodes.last()!!.next != rootNodes.first()) {
            rootNodes.add(rootNodes.last()?.next)
        }
        return rootNodes.requireNoNulls().joinToString { it.value.toString() }
    }
}

class FibonacciHeapTests {
    @Test fun `construct heap`() {
        val heap = FibonacciHeap<Int>()
        heap.add(3)
        heap.add(1)
        heap.add(2)
        heap.printed()
        heap.minValue() shouldEqual 1
    }
}