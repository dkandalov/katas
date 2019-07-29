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
        listNodes(1, 2, 3, 4, 5, 6, 7, 8).swapPairs() shouldEqual listNodes(2, 1, 4, 3, 6, 5, 8, 7)

        listNodes(1).swapPairs() shouldEqual listNodes(1)
        listNodes(1, 2, 3).swapPairs() shouldEqual listNodes(2, 1, 3)
        listNodes(1, 2, 3, 4, 5).swapPairs() shouldEqual listNodes(2, 1, 4, 3, 5)
    }
}

private fun ListNode.swapPairs(): ListNode {
    var first: ListNode? = this
    var second = first?.next
    var third = second?.next
    var fourth = third?.next
    if (second == null) return first!!
    val head = second

    second.next = first
    first?.next = fourth ?: third

    while (third != null) {
        first = third
        second = first.next
        third  = second?.next
        fourth = third?.next

        second?.next = first
        first.next = fourth ?: third
    }

    return head
}
