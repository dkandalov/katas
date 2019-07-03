package katas.kotlin.leetcode.addtwonumbers

import kotlincommon.test.shouldEqual
import org.junit.Test

class AddTwoNumbers {
    @Test fun `convert integer to linked list`() {
        0.toLinkedList() shouldEqual Node(0)
        1.toLinkedList() shouldEqual Node(1)
        12.toLinkedList() shouldEqual Node(2).linkedTo(Node(1))
        30.toLinkedList() shouldEqual Node(0).linkedTo(Node(3))
        321.toLinkedList() shouldEqual Node(1).linkedTo(Node(2).linkedTo(Node(3)))
    }

    @Test fun `convert linked list to string`() {
        Node(1).toString() shouldEqual "1"
        Node(2).linkedTo(Node(1)).toString() shouldEqual "2 -> 1"
        Node(2).linkedTo(Node(3).linkedTo(Node(4))).toString() shouldEqual "2 -> 3 -> 4"
    }

    @Test fun `add two numbers`() {
        Node(1) + Node(2) shouldEqual Node(3)

        Node(1) + Node(9) shouldEqual Node(0).linkedTo(Node(1))
        Node(9) + Node(1) shouldEqual Node(0).linkedTo(Node(1))
        Node(9) + Node(9) shouldEqual Node(8).linkedTo(Node(1))

        19.toLinkedList() + 9.toLinkedList() shouldEqual 28.toLinkedList()
        99.toLinkedList() + 9.toLinkedList() shouldEqual 108.toLinkedList()

        21.toLinkedList() + 43.toLinkedList() shouldEqual 64.toLinkedList()
        123.toLinkedList() + 456.toLinkedList() shouldEqual 579.toLinkedList()
        342.toLinkedList() + 465.toLinkedList() shouldEqual 807.toLinkedList()
    }
}

private fun Int.toLinkedList(): Node {
    val node = Node(this % 10)
    val n = this / 10
    return if (n == 0) node else node.linkedTo(n.toLinkedList())
}

private data class Node(val value: Int, val next: Node? = null) {
    fun linkedTo(that: Node?) = copy(next = that)

    operator fun plus(that: Node?): Node {
        if (that == null) return this

        val sum = value + that.value
        var nextSumNode =
            if (next == null && that.next == null) null
            else if (next == null) that.next
            else if (that.next == null) next
            else next + that.next

        if (sum >= 10) nextSumNode = Node(1) + nextSumNode

        return Node(sum % 10).linkedTo(nextSumNode)
    }

    override fun toString() = if (next == null) value.toString() else "$value -> $next"
}