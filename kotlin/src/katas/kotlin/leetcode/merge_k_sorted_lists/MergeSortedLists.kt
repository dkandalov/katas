package katas.kotlin.leetcode.merge_k_sorted_lists

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/merge-k-sorted-lists
 */
class MergeSortedListsTests {
    @Test fun `merge k sorted linked lists`() {
        merge(arrayOf(listNodes(1))) shouldEqual listNodes(1)
        merge(arrayOf(listNodes(1, 2))) shouldEqual listNodes(1, 2)
        merge(arrayOf(listNodes(1), listNodes(2))) shouldEqual listNodes(1, 2)

        merge(arrayOf(listNodes(1, 2), listNodes(3))) shouldEqual listNodes(1, 2, 3)
        merge(arrayOf(listNodes(1, 3), listNodes(2))) shouldEqual listNodes(1, 2, 3)

        merge(arrayOf(listNodes(1, 4, 5), listNodes(1, 3, 4), listNodes(2, 6))) shouldEqual listNodes(1, 1, 2, 3, 4, 4, 5, 6)
    }
}

private fun merge(listNodes: Array<ListNode?>): ListNode? {
    fun Array<ListNode?>.removeMin(): ListNode {
        var result: ListNode? = null
        var index = 0
        forEachIndexed { i, node ->
            if (node != null) {
                if (result == null || node.value < result!!.value) {
                    result = node
                    index = i
                }
            }
        }
        listNodes[index] = result!!.next
        return result!!
    }

    fun Array<ListNode?>.hasNodes() = any { it != null }

    if (!listNodes.hasNodes()) return null

    var result = listNodes.removeMin()
    result.next = null
    while (listNodes.hasNodes()) {
        val node = listNodes.removeMin()
        node.next = result
        result = node
    }
    return result.reverse()
}