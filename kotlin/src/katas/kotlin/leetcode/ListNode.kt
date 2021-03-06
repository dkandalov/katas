package katas.kotlin.leetcode

import datsok.shouldEqual
import org.junit.Test

data class ListNode(val value: Int, var next: ListNode? = null) {
    override fun toString(): String {
        val nextString = if (next == null) "" else "->$next"
        return value.toString() + nextString
    }

    fun toList(): List<Int> {
        return listOf(value) + (next?.toList() ?: emptyList())
    }

    fun reverse(): ListNode = doReverse().first

    private fun doReverse(): Pair<ListNode, ListNode> {
        if (next == null) return Pair(this, this)
        val (first, last) = next!!.doReverse()
        last.next = this
        next = null
        return Pair(first, this)
    }
}

fun List<Int>.toListNode(): ListNode {
    if (isEmpty()) error("List must be non-empty to convert it to ListNode")
    return foldRight(null as ListNode?, { n, result -> ListNode(n, result) })!!
}

fun listNodes(vararg values: Int): ListNode = values.toList().toListNode()

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
        listOf(1, 2).toListNode() shouldEqual ListNode(1, ListNode(2))
        listOf(1, 2, 3).toListNode() shouldEqual ListNode(1, ListNode(2, ListNode(3)))
    }

    @Test fun `reverse nodes`() {
        listNodes(1).reverse() shouldEqual listNodes(1)
        listNodes(1, 2).reverse() shouldEqual listNodes(2, 1)
        listNodes(1, 2, 3).reverse() shouldEqual listNodes(3, 2, 1)
    }
}
