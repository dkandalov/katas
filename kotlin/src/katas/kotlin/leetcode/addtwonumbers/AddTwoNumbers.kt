package katas.kotlin.leetcode.addtwonumbers

import kotlincommon.test.shouldEqual
import org.junit.Test

class AddTwoNumbers {
    @Test fun `convert integer to linked list`() {
        0.toLinkedList() shouldEqual Node(0)
        1.toLinkedList() shouldEqual Node(1)
        12.toLinkedList() shouldEqual Node(2).linkedTo(Node(1))
        30.toLinkedList() shouldEqual Node(0).linkedTo(Node(3))
    }

    @Test fun `convert linked list to string`() {
        Node(2).linkedTo(Node(1)).toString() shouldEqual "2 -> 1"
        Node(2).linkedTo(Node(3).linkedTo(Node(4))).toString() shouldEqual "2 -> 3 -> 4"
    }
}

private fun Int.toLinkedList(): Node {
    val node = Node(this % 10)
    val n = this / 10
    return if (n == 0) node else node.linkedTo(n.toLinkedList())
}

private data class Node(val value: Int, val next: Node? = null) {
    fun linkedTo(that: Node) = copy(next = that)

    override fun toString() = if (next == null) value.toString() else "$value -> $next"
}