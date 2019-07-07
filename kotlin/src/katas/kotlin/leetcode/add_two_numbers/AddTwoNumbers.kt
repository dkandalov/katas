package katas.kotlin.leetcode.add_two_numbers

import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import kotlin.random.Random

/**
 * You are given two non-empty linked lists representing two non-negative integers.
 * The digits are stored in reverse order and each of their nodes contain a single digit.
 * Add the two numbers and return it as a linked list.
 *
 * You may assume the two numbers do not contain any leading zero, except the number 0 itself.
 *
 * https://leetcode.com/problems/add-two-numbers
 */
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

    @Test fun `add random positive numbers`() {
        val random = Random(seed = Random.nextInt().printed("seed: "))
        val a = random.nextInt(0, 1_000_000)
        val b = random.nextInt(0, 1_000_000)
        a.toLinkedList() + b.toLinkedList() shouldEqual (a + b).toLinkedList()
    }
}

private fun Int.toLinkedList(): Node {
    val node = Node(this % 10)
    val n = this / 10
    return if (n == 0) node else node.linkedTo(n.toLinkedList())
}

private data class Node(val value: Int, val next: Node? = null) {
    fun linkedTo(that: Node?) = copy(next = that)

    operator fun plus(that: Node): Node {
        var nextSumNode =
            if (next == null && that.next == null) null
            else if (next == null) that.next
            else if (that.next == null) next
            else next + that.next

        val sum = value + that.value
        if (sum >= 10) nextSumNode = if (nextSumNode == null) Node(1) else nextSumNode + Node(1)

        return Node(sum % 10).linkedTo(nextSumNode)
    }

    override fun toString() = if (next == null) value.toString() else "$value -> $next"
}