package katas.kotlin.leetcode.reverse_nodes_in_k_group

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import kotlincommon.test.shouldEqual
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

private fun ListNode.reverseGroup(size: Int): ListNode {
    val window = arrayOfNulls<ListNode?>(size)
    fun writeToWindow(listNode: ListNode) {
        window[0] = listNode
        (1 until window.size).forEach { window[it] = window[it - 1]?.next }
    }

    fun reverseWindow() {
        window[0]?.next = null
        (1 until size).forEach { window[it]?.next = window[it - 1] }
    }

    var node: ListNode? = this
    var head: ListNode? = null
    var newLast: ListNode? = null

    while (node != null) {
        writeToWindow(node)
        node = window.last()?.next
        if (head == null) head = window.last()
        if (window.any { it == null }) {
            newLast?.next = window[0]
            break
        }

        reverseWindow()
        newLast?.next = window.last()
        newLast = window[0]
    }

    return head!!
}
