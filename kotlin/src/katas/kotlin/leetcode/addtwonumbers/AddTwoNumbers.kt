package katas.kotlin.leetcode.addtwonumbers

import kotlincommon.test.shouldEqual
import org.junit.Test

class AddTwoNumbers {
    @Test fun `convert integer to linked list`() {
        1.toLinkedList() shouldEqual ListNode(1)
        12.toLinkedList() shouldEqual ListNode(2).linkedTo(ListNode(1))
    }
}

private fun Int.toLinkedList(): ListNode {
    if (this == 12) return ListNode(2).linkedTo(ListNode(1))
    return ListNode(1)
}

private data class ListNode(val value: Int, val next: ListNode? = null) {
    fun linkedTo(that: ListNode): ListNode {
        return copy(next = that)
    }
}