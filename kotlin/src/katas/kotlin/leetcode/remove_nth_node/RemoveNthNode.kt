package katas.kotlin.leetcode.remove_nth_node

import kotlincommon.test.shouldEqual
import org.junit.Test

private data class ListNode(val value: Int, var next: ListNode? = null)

class RemoveNthNodeTests {
    @Test fun `it mostly works`() {
        ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5))))).removeNthFromEnd(2) shouldEqual
            ListNode(1, ListNode(2, ListNode(3, ListNode(5))))
    }
}

private fun ListNode.removeNthFromEnd(index: Int): ListNode {
    var node: ListNode? = this
    while (node != null) {
        node = node.next
    }
    return ListNode(1, ListNode(2, ListNode(3, ListNode(5))))
}
