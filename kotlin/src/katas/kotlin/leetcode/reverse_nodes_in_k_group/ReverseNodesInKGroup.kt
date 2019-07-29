package katas.kotlin.leetcode.reverse_nodes_in_k_group

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import kotlincommon.test.shouldEqual
import org.junit.Test

class ReverseNodesInKGroupTests {
    @Test fun `reverse the nodes of a linked list k at a time`() {
//        listNodes(1, 2, 3).reverseGroup(1) shouldEqual listNodes(1, 2, 3)
        listNodes(1, 2, 3).reverseGroup(2) shouldEqual listNodes(2, 1, 3)
//        listNodes(1, 2, 3).reverseGroup(3) shouldEqual listNodes(3, 2, 1)
    }
}

private fun ListNode.reverseGroup(size: Int): ListNode {
    val first = this
    val second = first.next
    val third = second?.next
    second?.next = first
    first.next = third
    return second!!
}
