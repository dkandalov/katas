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
    var tailSize = 0
    val tail = ArrayList<ListNode>()
    while (node != null) {
        tail.add(node)
        tailSize++
        node = node.next
    }
    tail[tail.size - index - 1].next = if (index == 1) null else tail[tail.size - index + 1]
    return this
}
