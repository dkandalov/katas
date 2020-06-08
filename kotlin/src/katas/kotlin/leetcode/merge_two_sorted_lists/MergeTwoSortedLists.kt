package katas.kotlin.leetcode.merge_two_sorted_lists

import katas.kotlin.leetcode.ListNode
import katas.kotlin.leetcode.listNodes
import katas.kotlin.leetcode.toListNode
import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/merge-two-sorted-lists/
 */
class MergeTwoSortedListsTests {
    @Test fun `do the merge`() {
        listNodes(1) + listNodes(2) shouldEqual listNodes(1, 2)
        listOf(1, 2, 4).toListNode() + listOf(1, 3, 4).toListNode()
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
        if (left.value <= right.value) {
            result.next = left
            left = left.next
        } else {
            result.next = right
            right = right.next
        }
    }
    if (left != null) result.next = left
    if (right != null) result.next = right
    return result
}
