package katas.kotlin.leetcode.merge_k_sorted_lists

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import kotlincommon.test.shouldEqual
import org.junit.Test

class MergeSortedListsTests {
    @Test fun `merge k sorted linked lists`() {
        merge(arrayOf(listNodes(1))) shouldEqual ListNode(1)
//        merge(arrayOf(listNodes(1, 4, 5), listNodes(1, 3, 4), listNodes(2, 6)))
    }
}

private fun merge(listNodes: Array<ListNode?>): ListNode? {
    fun Array<ListNode?>.min(): Pair<ListNode?, Int> {
        var result: ListNode? = null
        var index = 0
        forEachIndexed { i, node ->
            if (node != null) {
                if (result == null) result = node
                else if (node.value < result!!.value) {
                    result = node
                    index = i
                }
            }
        }
        return result to index
    }
    fun Array<ListNode?>.hasNodes() = any { it != null }

    var (result, i) = listNodes.min()
    listNodes[i] = result?.next
    while (listNodes.hasNodes()) {
        listNodes.min().let {
            result!!.next = it.first
            i = it.second
        }
        listNodes[i] = result?.next
    }
    return result!!
}