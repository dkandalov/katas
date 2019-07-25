package katas.kotlin.leetcode.remove_nth_node

import org.junit.Test

class ListNode(val value: Int, var next: ListNode? = null)

class RemoveNthNodeTests {
    @Test fun `foo`() {
        ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
    }
}