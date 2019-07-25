package katas.kotlin.leetcode.merge_two_sorted_lists

import katas.kotlin.leetcode.remove_nth_node.ListNode
import katas.kotlin.leetcode.remove_nth_node.toListNode
import org.junit.Test

/**
 * https://leetcode.com/problems/merge-two-sorted-lists/
 */
class MergeTwoSortedListsTests {
    @Test fun `do the merge`() {
        listOf(1, 2, 4).toListNode() + listOf(1, 3, 4).toListNode()
    }
}

private operator fun ListNode.plus(listNode: ListNode): ListNode {
    return this
}
