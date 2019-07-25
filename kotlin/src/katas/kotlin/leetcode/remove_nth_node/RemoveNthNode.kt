package katas.kotlin.leetcode.remove_nth_node

import kotlincommon.test.shouldEqual
import org.junit.Test

data class ListNode(val value: Int, var next: ListNode? = null) {
    override fun toString(): String {
        val nextString = if (next == null) "" else "->$next"
        return value.toString() + nextString
    }

    fun toList(): List<Int> {
        return listOf(value) + (next?.toList() ?: emptyList())
    }
}

fun List<Int>.toListNode(): ListNode {
    if (isEmpty()) error("")
    val result = ListNode(first())
    drop(1).forEach { result.next = ListNode(it) }
    return result
}


class ListNodeTests {
    @Test fun `conversion to string`() {
        ListNode(1).toString() shouldEqual "1"
        ListNode(1, ListNode(2)).toString() shouldEqual "1->2"
        ListNode(1, ListNode(2, ListNode(3))).toString() shouldEqual "1->2->3"
    }

    @Test fun `conversion to list`() {
        ListNode(1).toList() shouldEqual listOf(1)
        ListNode(1, ListNode(2)).toList() shouldEqual listOf(1, 2)
        ListNode(1, ListNode(2, ListNode(3))).toList() shouldEqual listOf(1, 2, 3)
    }

    @Test fun `conversion from list`() {
        listOf(1).toListNode() shouldEqual ListNode(1)
        ListNode(1, ListNode(2)).toList() shouldEqual listOf(1, 2)
        ListNode(1, ListNode(2, ListNode(3))).toList() shouldEqual listOf(1, 2, 3)
    }
}

class RemoveNthNodeTests {
    @Test fun `it mostly works`() {
        ListNode(1).removeNthFromEnd(1) shouldEqual null

        ListNode(1, ListNode(2)).removeNthFromEnd(1) shouldEqual ListNode(1)
        ListNode(1, ListNode(2)).removeNthFromEnd(2) shouldEqual ListNode(2)

        ListNode(1, ListNode(2, ListNode(3))).removeNthFromEnd(1) shouldEqual ListNode(1, ListNode(2))
        ListNode(1, ListNode(2, ListNode(3))).removeNthFromEnd(2) shouldEqual ListNode(1, ListNode(3))
        ListNode(1, ListNode(2, ListNode(3))).removeNthFromEnd(3) shouldEqual ListNode(2, ListNode(3))

        ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5))))).removeNthFromEnd(1) shouldEqual ListNode(1, ListNode(2, ListNode(3, ListNode(4))))
        ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5))))).removeNthFromEnd(2) shouldEqual
            ListNode(1, ListNode(2, ListNode(3, ListNode(5))))
    }
}

private fun ListNode.removeNthFromEnd(index: Int): ListNode? {
    var node: ListNode? = this
    var tailSize = 0
    var tail = this
    while (node != null) {
        tailSize++
        if (tailSize > index + 1) tail = tail.next!!
        node = node.next
    }
    if (tailSize == index) return tail.next
    tail.next = tail.next?.next
    return this
}
