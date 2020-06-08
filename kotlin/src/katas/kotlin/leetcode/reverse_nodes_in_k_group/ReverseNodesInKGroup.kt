package katas.kotlin.leetcode.reverse_nodes_in_k_group

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import datsok.shouldEqual
import org.junit.Test

class ReverseNodesInKGroupTests {
    @Test fun `reverse the nodes of a linked list k at a time`() {
        listNodes(1).reverseGroup(1) shouldEqual listNodes(1)
        listNodes(1, 2).reverseGroup(1) shouldEqual listNodes(1, 2)
        listNodes(1, 2, 3).reverseGroup(1) shouldEqual listNodes(1, 2, 3)

        listNodes(1, 2).reverseGroup(2) shouldEqual listNodes(2, 1)
        listNodes(1, 2, 3).reverseGroup(2) shouldEqual listNodes(2, 1, 3)
        listNodes(1, 2, 3, 4).reverseGroup(2) shouldEqual listNodes(2, 1, 4, 3)

        listNodes(1, 2, 3).reverseGroup(3) shouldEqual listNodes(3, 2, 1)
        listNodes(1, 2, 3, 4).reverseGroup(3) shouldEqual listNodes(3, 2, 1, 4)
        listNodes(1, 2, 3, 4, 5).reverseGroup(3) shouldEqual listNodes(3, 2, 1, 4, 5)
        listNodes(1, 2, 3, 4, 5, 6).reverseGroup(3) shouldEqual listNodes(3, 2, 1, 6, 5, 4)
        listNodes(1, 2, 3, 4, 5, 6, 7, 8, 9).reverseGroup(3) shouldEqual listNodes(3, 2, 1, 6, 5, 4, 9, 8, 7)
    }
}

private fun Array<ListNode?>.writeListNodes(listNode: ListNode) {
    this[0] = listNode
    (1 until size).forEach { this[it] = this[it - 1]?.next }
}

private fun Array<ListNode?>.reverseListNodes() {
    this[0]?.next = null
    (1 until size).forEach { this[it]?.next = this[it - 1] }
}

private inline fun slidingWindow(listNode: ListNode, size: Int, f: (Array<ListNode?>) -> Unit) {
    val window = arrayOfNulls<ListNode?>(size)
    var node: ListNode? = listNode
    while (node != null) {
        window.writeListNodes(node)
        node = window.last()?.next
        f(window)
    }
}

private fun ListNode.reverseGroup(size: Int): ListNode {
    var result: ListNode? = null
    var lastTip: ListNode? = null

    slidingWindow(this, size) { window ->
        if (result == null) result = window.last()
        if (window.any { it == null }) {
            lastTip?.next = window[0]
            return result!!
        }

        window.reverseListNodes()

        lastTip?.next = window.last()
        lastTip = window[0]
    }

    return result!!
}
