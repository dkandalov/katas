package katas.kotlin.leetcode.reverse_nodes_in_k_group

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import kotlincommon.test.shouldEqual
import org.junit.Test

class ReverseNodesInKGroupTests {
    @Test fun `reverse the nodes of a linked list k at a time`() {
        listNodes(1, 2, 3).reverseGroup(1) shouldEqual listNodes(1, 2, 3)
    }
}

private fun ListNode.reverseGroup(size: Int): ListNode {
    return this
}
