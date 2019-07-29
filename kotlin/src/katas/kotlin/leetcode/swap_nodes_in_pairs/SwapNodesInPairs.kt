package katas.kotlin.leetcode.swap_nodes_in_pairs

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/swap-nodes-in-pairs
 */
class SwapNodesInPairsTests {
    @Test fun `swap every two adjacent nodes`() {
        listNodes(1, 2).swapPairs() shouldEqual listNodes(2, 1)
        listNodes(1, 2, 3, 4).swapPairs() shouldEqual listNodes(2, 1, 4, 3)
        listNodes(1, 2, 3, 4, 5, 6).swapPairs() shouldEqual listNodes(2, 1, 4, 3, 6, 5)
    }
}

private fun ListNode.swapPairs(): ListNode {
    var first: ListNode? = this
    var second = first?.next
    var third = first?.next?.next
    val head = second

    second?.next = first
    first?.next = third?.next

    while (third != null) {
        first = third
        second = first.next!!
        third  = second.next

        second.next = first
        first.next = third?.next
    }

    return head!!
}
