package katas.kotlin.leetcode.merge_k_sorted_lists

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import katas.kotlin.leetcode.toListNode
import kotlincommon.test.shouldEqual
import org.junit.Test

class MergeSortedListsTests {
    @Test fun `merge k sorted linked lists`() {
        merge(arrayOf(listNodes(1))) shouldEqual listNodes(1)
        merge(arrayOf(listNodes(1, 2))) shouldEqual listNodes(1, 2)
        merge(arrayOf(listNodes(1), listNodes(2))) shouldEqual listNodes(1, 2)

        merge(arrayOf(listNodes(1, 2), listNodes(3))) shouldEqual listNodes(1, 2, 3)
        merge(arrayOf(listNodes(1, 3), listNodes(2))) shouldEqual listNodes(1, 2, 3)

//        merge(arrayOf(listNodes(1, 4, 5), listNodes(1, 3, 4), listNodes(2, 6)))
    }
}

private fun merge(listNodes: Array<ListNode?>): ListNode? {
    fun Array<ListNode?>.removeMin(): ListNode? {
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
        return result
    }

    fun Array<ListNode?>.hasNodes() = any { it != null }

    val result = ArrayList<Int>()
    while (listNodes.hasNodes()) {
        listNodes.removeMin().let { node ->
            result.add(node!!.value)
        }
    }
    return result.toListNode()
}