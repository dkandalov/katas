package katas.kotlin.sort.heapsort

import kotlincommon.permutations
import kotlincommon.test.shouldEqual
import org.junit.Test

class HeapSort0Tests {
    @Test fun `sort a list`() {
        emptyList<Int>().heapSort() shouldEqual emptyList()
        listOf(1).heapSort() shouldEqual listOf(1)

        listOf(1, 2, 3).permutations().forEach {
            it.heapSort() shouldEqual listOf(1, 2, 3)
        }
    }

    @Test fun `remove smallest element from heap`() {
        val heap = Heap(listOf(1, 2, 0))

        heap.removeTop() shouldEqual 0
        heap.isEmpty() shouldEqual false

        heap.removeTop() shouldEqual 1
        heap.isEmpty() shouldEqual false

        heap.removeTop() shouldEqual 2
        heap.isEmpty() shouldEqual true
    }
}

private fun <E: Comparable<E>> List<E>.heapSort(): List<E> {
    val result = ArrayList<E>()
    val heap = Heap(this)
    while (!heap.isEmpty()) {
        result.add(heap.removeTop())
    }
    return result
}

class Heap<T: Comparable<T>>(elements: List<T>) {
    private val data = elements.toMutableList()

    fun removeTop(): T {
        val element = data.min()!!
        data.remove(element)
        return element
    }

    fun isEmpty(): Boolean {
        return data.isEmpty()
    }
}
