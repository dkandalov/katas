package katas.kotlin.leetcode.add_two_numbers

import datsok.shouldEqual
import katas.kotlin.leetcode.ListNode
import nonstdlib.with
import org.junit.jupiter.api.Test

class AddTwoNumbers_2_Tests {
    @Test fun `convert number to ListNode`() {
        1.toListNode() shouldEqual ListNode(1)
        10.toListNode() shouldEqual ListNode(0, ListNode(1))
        12.toListNode() shouldEqual ListNode(2, ListNode(1))
        100.toListNode() shouldEqual ListNode(0, ListNode(0, ListNode(1)))
        102.toListNode() shouldEqual ListNode(2, ListNode(0, ListNode(1)))
        123.toListNode() shouldEqual ListNode(3, ListNode(2, ListNode(1)))
        321.toListNode() shouldEqual ListNode(1, ListNode(2, ListNode(3)))
    }

    @Test fun `it works`() {
        addTwoNumbers(0.toListNode(), 0.toListNode()) shouldEqual 0.toListNode()
        addTwoNumbers(1.toListNode(), 2.toListNode()) shouldEqual 3.toListNode()
        addTwoNumbers(10.toListNode(), 30.toListNode()) shouldEqual 40.toListNode()
        addTwoNumbers(12.toListNode(), 3.toListNode()) shouldEqual 15.toListNode()
        addTwoNumbers(342.toListNode(), 465.toListNode()) shouldEqual 807.toListNode()
        addTwoNumbers(1342.toListNode(), 465.toListNode()) shouldEqual 1807.toListNode()
        addTwoNumbers(465.toListNode(), 1342.toListNode()) shouldEqual 1807.toListNode()
    }
}

fun addTwoNumbers(l1: ListNode?, l2: ListNode?): ListNode? {
    var (longest, shortest) =
        if (l1.size() >= l2.size()) Pair(l1, l2)
        else Pair(l2, l1)

    var carry = false
    var head: ListNode? = null
    var tip: ListNode? = null
    do {
        val sum = longest?.value!! + (shortest?.value ?: 0) + if (carry) 1 else 0
        val newElement = ListNode(sum % 10)
        if (head == null) {
            head = newElement
            tip = newElement
        } else {
            tip?.next = newElement
            tip = newElement
        }
        carry = sum >= 10
        longest = longest.next
        shortest = shortest?.next
    } while (longest != null)
    return head
}

fun ListNode?.size(): Int =
    if (this == null) 0 else 1 + next.size()

private fun Int.toListNode(): ListNode =
    if (this < 10) ListNode(this)
    else (this % 10).toListNode().with {
        it.next = (this / 10).toListNode()
    }
