package katas.kotlin.skiena

fun <T : Comparable<T>> List<T>.toHeap(): Heap<T> {
    val heap = Heap<T>(size)
    forEach { heap.add(it) }
    return heap
}

class Heap<T : Comparable<T>>(private val size: Int = 32) {
    @Suppress("UNCHECKED_CAST")
    private var array: Array<T?> = arrayOfNulls<Any?>(size + 1) as Array<T?>
    private var n = 0

    fun add(item: T) {
        if (n >= size) error("Heap overflow")
        n++
        array[n] = item
        siftUp(n)
    }

    fun removeMin(): T {
        if (n <= 1) error("Empty heap")
        val min = array[1]!!

        array[1] = array[n]
        n--
        siftDown(1)

        return min
    }

    fun isGreaterOrEqual(atIndex: Int, item: T) {
        compare(1, atIndex, item)
    }

    private fun compare(i: Int, count: Int, item: T): Int {
        if (count <= 0 || i > n) return count

        var result = count
        if (array[i]!! < item) {
            result = compare(i.child1, result - 1, item)
            result = compare(i.child2, result, item)
        }
        return result
    }

    @Suppress("UNCHECKED_CAST")
    fun toList(): List<T> = array.take(n) as List<T>

    private fun siftDown(index: Int) {
        var minIndex = index
        if (index.child1 <= n && array[index.child1]!! < array[minIndex]!!) minIndex = index.child1
        if (index.child2 <= n && array[index.child2]!! < array[minIndex]!!) minIndex = index.child2

        swap(index, minIndex)
        siftDown(minIndex)
    }

    private fun siftUp(index: Int) {
        if (index.parent == -1) return
        if (array[index.parent]!! > array[index]!!) {
            swap(index.parent, index)
            siftUp(index.parent)
        }
    }

    private val Int.child1: Int get() = this * 2
    private val Int.child2: Int get() = this * 2 + 1
    private val Int.parent: Int get() = if (this == 1) -1 else this / 2

    private fun swap(index1: Int, index2: Int) {
        val tmp = array[index1]
        array[index1] = array[index2]
        array[index2] = tmp
    }

    companion object {
        fun <T : Comparable<T>> make(list: List<T>): Heap<T> {
            val heap = Heap<T>(list.size)
            list.forEachIndexed { i, item ->
                heap.array[i + 1] = item
            }
            heap.n.downTo(1).forEach { i ->
                heap.siftDown(i)
            }
            return heap
        }
    }
}