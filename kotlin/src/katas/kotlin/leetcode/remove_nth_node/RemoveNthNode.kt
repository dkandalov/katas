package katas.kotlin.leetcode.remove_nth_node

import kotlincommon.test.shouldEqual
import org.junit.Test

private data class ListNode(val value: Int, var next: ListNode? = null)

class RemoveNthNodeTests {
    @Test fun `it mostly works`() {
        ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5))))).removeNthFromEnd(1) shouldEqual ListNode(1, ListNode(2, ListNode(3, ListNode(4))))
        ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5))))).removeNthFromEnd(2) shouldEqual
            ListNode(1, ListNode(2, ListNode(3, ListNode(5))))
    }
}

private fun ListNode.removeNthFromEnd(index: Int): ListNode {
    var node: ListNode? = this
    var tailSize = 0
    var tail = this
    while (node != null) {
        tailSize++
        if (tailSize > index + 1) tail = tail.next!!
        node = node.next
    }
    tail.next = if (tailSize == 1) null else tail.next!!.next
    return this
}
