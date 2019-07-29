package katas.kotlin.leetcode.reverse_nodes_in_k_group

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import kotlincommon.test.shouldEqual
import org.junit.Test

class ReverseNodesInKGroupTests {
    @Test fun `reverse the nodes of a linked list k at a time`() {
//        listNodes(1, 2, 3).reverseGroup(1) shouldEqual listNodes(1, 2, 3)
//        listNodes(1, 2, 3).reverseGroup(2) shouldEqual listNodes(2, 1, 3)
        listNodes(1, 2, 3).reverseGroup(3) shouldEqual listNodes(3, 2, 1)
    }
}

private fun ListNode.reverseGroup(size: Int): ListNode {
    val window = arrayOfNulls<ListNode?>(size)
    window[0] = this
    (1 until window.size).forEach { window[it] = window[it - 1]?.next }
    var afterWindow = window.last()?.next

    window[0]?.next = null
    (1 until size).forEach { window[it]?.next = window[it - 1] }
    var newLast = window[0]

    if (afterWindow != null) {
        window[0] = afterWindow
        (1 until window.size).forEach { window[it] = window[it - 1]?.next }
        afterWindow = window.last()?.next

        window[0]?.next = null
        (1 until size).forEach { window[it]?.next = window[it - 1] }
        newLast?.next = window.last()
        newLast = window[0]
    }

    return window.last()!!
}
