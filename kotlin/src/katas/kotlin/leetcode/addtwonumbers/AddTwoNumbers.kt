package katas.kotlin.leetcode.addtwonumbers

import kotlincommon.test.shouldEqual
import org.junit.Test

class AddTwoNumbers {
    @Test fun `convert integer to linked list`() {
        0.toLinkedList() shouldEqual ListNode(0)
        1.toLinkedList() shouldEqual ListNode(1)
        12.toLinkedList() shouldEqual ListNode(2).linkedTo(ListNode(1))
        30.toLinkedList() shouldEqual ListNode(0).linkedTo(ListNode(3))
    }

    @Test fun `convert linked list to string`() {
        ListNode(2).linkedTo(ListNode(1)).toString() shouldEqual "2 -> 1"
        ListNode(2).linkedTo(ListNode(3).linkedTo(ListNode(4))).toString() shouldEqual "2 -> 3 -> 4"
    }
}

private fun Int.toLinkedList(): ListNode {
    val node = ListNode(this % 10)
    val n = this / 10
    return if (n == 0) node else node.linkedTo(n.toLinkedList())
}

private data class ListNode(val value: Int, val next: ListNode? = null) {
    fun linkedTo(that: ListNode) = copy(next = that)
    fun linkedTo(n: Int) = copy(next = ListNode(n))

    override fun toString() = if (next == null) value.toString() else "$value -> $next"
}