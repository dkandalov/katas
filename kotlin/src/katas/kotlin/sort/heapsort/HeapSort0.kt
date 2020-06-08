package katas.kotlin.sort.heapsort

import nonstdlib.listOfInts
import nonstdlib.permutations
import nonstdlib.printed
import nonstdlib.swap
import datsok.shouldEqual
import org.junit.Test
import kotlin.random.Random

class HeapSort0Tests {
    @Test fun `sort a list`() {
        emptyList<Int>().heapSort() shouldEqual emptyList()
        listOf(1).heapSort() shouldEqual listOf(1)

        listOf(1, 2, 3).permutations().forEach {
            it.heapSort() shouldEqual listOf(1, 2, 3)
        }

        listOf(1, 2, 3, 4).permutations().forEach {
            it.heapSort() shouldEqual listOf(1, 2, 3, 4)
        }

        fun List<Int>.isSorted() =
            windowed(size = 2).all { it[0] <= it[1] }

        val list = Random(seed = Random.nextInt().printed()).listOfInts(
            sizeRange = 0..100,
            valuesRange = 0..100
        ).printed().heapSort()
        list.printed().isSorted() shouldEqual true
    }

    @Test fun `array-based binary heap`() {
        val heap = BinaryHeap()

        heap.add(2)
        heap.add(1)
        heap.add(3)
        heap.add(-1)

        heap.removeTop() shouldEqual -1
        heap.removeTop() shouldEqual 1
        heap.removeTop() shouldEqual 2
        heap.removeTop() shouldEqual 3
    }
}

class BinaryHeap(list: List<Int> = emptyList()) {
    private val array = Array(size = 256, init = { 0 })
    private var size = 0

    init {
        list.forEach { add(it) }
    }

    fun add(element: Int) {
        array[size] = element
        siftUp(size)
        size++
    }

    fun removeTop(): Int {
        val result = array[0]
        array[0] = array[size - 1]
        size--
        siftDown(0)
        return result
    }

    private fun siftUp(index: Int) {
        val parentIndex = if (index == 0) -1 else (index - 1) / 2
        if (parentIndex != -1 && array[parentIndex] > array[index]) {
            array.swap(parentIndex, index)
            siftUp(parentIndex)
        }
    }

    private fun siftDown(index: Int) {
        var minIndex = index
        val childIndex1 = index * 2 + 1
        val childIndex2 = index * 2 + 2
        if (childIndex1 < size && array[childIndex1] < array[minIndex]) minIndex = childIndex1
        if (childIndex2 < size && array[childIndex2] < array[minIndex]) minIndex = childIndex2

        if (minIndex == index) return
        array.swap(index, minIndex)
        siftDown(minIndex)
    }

    fun isEmpty(): Boolean = size == 0
}

private fun List<Int>.heapSort(): List<Int> {
    val result = ArrayList<Int>()
    val heap = BinaryHeap(this)
    while (!heap.isEmpty()) {
        result.add(heap.removeTop())
    }
    return result
}
