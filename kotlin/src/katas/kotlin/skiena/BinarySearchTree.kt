package katas.kotlin.skiena

import kotlincommon.printed

data class Node<T: Comparable<T>>(
    val item: T,
    var left: Node<T>? = null,
    var right: Node<T>? = null
) {
    fun search(item: T): Node<T>? {
        return if (this.item == item) this
        else if (this.item < item) left?.search(item)
        else right?.search(item)
    }

    fun findMinimum(): Node<T> {
        var min = this
        while (min.left != null) {
            min = min.left!!
        }
        return min
    }

    fun traverseTree(processItem: (T) -> Unit) {
        left?.traverseTree(processItem)
        processItem(item)
        right?.traverseTree(processItem)
    }

    fun insert(item: T) {
        if (item < this.item) {
            if (left == null) left = Node(item)
            else left!!.insert(item)
        } else {
            if (right == null) right = Node(item)
            else right!!.insert(item)
        }
    }
    
    override fun toString(): String {
        val childrenString = if (left == null && right == null) ""
        else {
            val leftString = if (left == null) "_" else left.toString()
            val rightString = if (right == null) "_" else right.toString()
            " $leftString $rightString"
        }
        return "($item$childrenString)"
    }

    companion object {
            fun <T: Comparable<T>> insertTree(node: Node<T>?, item: T, parent: Node<T>) {
            if (node == null) {
                Node(item, parent)
                // no way to "link into parent’s record"
                return
            }

            if (item < node.item) {
                if (node.left == null) {
                    node.left = Node(item, parent)
                } else {
                    insertTree(node.left, item, node)
                }
            } else {
                insertTree(node.right, item, node)
            }
        }
    }
}

fun <T: Comparable<T>> List<T>.toBST(): Node<T> =
    Node(first()).apply {
        drop(1).forEach { insert(it) }
    }


fun main(args: Array<String>) {
    listOf(1, 2, 3, 4, 5).toBST().printed().let { bst ->
        bst.traverseTree { println(it) }
    }
}