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
    var result = ListNode(this % 10)
    var n = this / 10
    while (n != 0) {
        result = result.linkedTo(ListNode(n % 10))
        n /= 10
    }
    return result
}

private data class ListNode(val value: Int, val next: ListNode? = null) {
    fun linkedTo(that: ListNode): ListNode {
        return copy(next = that)
    }
}