package katas.kotlin.leetcode.merge_two_sorted_lists

import katas.kotlin.leetcode.remove_nth_node.ListNode
import katas.kotlin.leetcode.remove_nth_node.toListNode
import org.junit.Test

/**
 * https://leetcode.com/problems/merge-two-sorted-lists/
 */
class MergeTwoSortedListsTests {
    @Test fun `do the merge`() {
        ListNode(1) + ListNode(2)
//        listOf(1, 2, 4).toListNode() + listOf(1, 3, 4).toListNode()
    }
}

private operator fun ListNode.plus(that: ListNode): ListNode {
    var left: ListNode? = this
    var right: ListNode? = that
    val result = if (value <= that.value) {
        left = left?.next
        this
    } else {
        right = right?.next
        that
    }
    while (left != null && right != null) {
        // ...
    }
    return result
}
