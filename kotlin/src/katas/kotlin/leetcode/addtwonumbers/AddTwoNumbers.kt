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
}

private fun Int.toLinkedList(): ListNode {
    val node = ListNode(this % 10)
    val n = this / 10
    return if (n == 0) node else node.linkedTo(n.toLinkedList())
}

private data class ListNode(val value: Int, val next: ListNode? = null) {
    fun linkedTo(that: ListNode): ListNode {
        return copy(next = that)
    }
}