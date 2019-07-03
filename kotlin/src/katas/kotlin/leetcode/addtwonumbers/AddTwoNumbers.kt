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
    val result = ListNode(this % 10)
    val n = this / 10
    return if (n == 0) result else result.linkedTo(n.toLinkedList())
}

private data class ListNode(val value: Int, val next: ListNode? = null) {
    fun linkedTo(that: ListNode): ListNode {
        return copy(next = that)
    }
}