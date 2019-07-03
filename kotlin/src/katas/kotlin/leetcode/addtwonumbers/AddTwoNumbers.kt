package katas.kotlin.leetcode.addtwonumbers

import kotlincommon.test.shouldEqual
import org.junit.Test

class AddTwoNumbers {
    @Test fun `convert integer to linked list`() {
        1.toLinkedList() shouldEqual ListNode(1)
    }
}

private fun Int.toLinkedList(): ListNode {
    return ListNode(1)
}

private data class ListNode(val value: Int)