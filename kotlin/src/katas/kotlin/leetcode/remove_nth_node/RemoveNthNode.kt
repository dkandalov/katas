package katas.kotlin.leetcode.remove_nth_node

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/remove-nth-node-from-end-of-list/
 */
class RemoveNthNodeTests {
    @Test fun `it mostly works`() {
        listNodes(1).removeNthFromEnd(1) shouldEqual null

        listNodes(1, 2).removeNthFromEnd(1) shouldEqual listNodes(1)
        listNodes(1, 2).removeNthFromEnd(2) shouldEqual listNodes(2)

        listNodes(1, 2, 3).removeNthFromEnd(1) shouldEqual listNodes(1, 2)
        listNodes(1, 2, 3).removeNthFromEnd(2) shouldEqual listNodes(1, 3)
        listNodes(1, 2, 3).removeNthFromEnd(3) shouldEqual listNodes(2, 3)

        listNodes(1, 2, 3, 4, 5).removeNthFromEnd(1) shouldEqual listNodes(1, 2, 3, 4)
        listNodes(1, 2, 3, 4, 5).removeNthFromEnd(2) shouldEqual listNodes(1, 2, 3, 5)
    }
}

private fun ListNode.removeNthFromEnd(index: Int): ListNode? {
    var node: ListNode? = this
    var tailSize = 0
    var tail = this
    while (node != null) {
        tailSize++
        if (tailSize > index + 1) tail = tail.next!!
        node = node.next
    }
    if (tailSize == index) return tail.next
    tail.next = tail.next?.next
    return this
}
