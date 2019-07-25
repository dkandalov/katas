package katas.kotlin.leetcode.remove_nth_node

import org.junit.Test

class ListNode(val value: Int, var next: ListNode? = null)

class RemoveNthNodeTests {
    @Test fun `it mostly works`() {
        ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5))))).removeNthFromEnd(2)
    }
}

private fun ListNode.removeNthFromEnd(index: Int): ListNode {
    return this
}
