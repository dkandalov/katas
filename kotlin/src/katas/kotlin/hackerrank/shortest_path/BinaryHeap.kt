package katas.kotlin.hackerrank.shortest_path

import katas.kotlin.hackerrank.shortest_path.BinaryHeap.Companion.binaryHeapOf
import katas.kotlin.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.ArrayList
import kotlin.collections.HashMap

class BinaryHeap<E>(
    private val comparator: Comparator<E> = defaultComparator(),
    initialCapacity: Int = 32
) {
    @Suppress("UNCHECKED_CAST")
    private var array: Array<E?> = arrayOfNulls<Any?>(size = initialCapacity) as Array<E?>
    private val indexByValue: MutableMap<E, Int> = HashMap(initialCapacity)
    private var size = 0

    fun size(): Int = size

    fun min(): E =
        if (size == 0) throw NoSuchElementException()
        else this[0]

    fun add(value: E) {
        if (indexByValue.containsKey(value)) return
        if (size == array.size) {
            array = Arrays.copyOf(array, array.size * 2)
        }
        this[size] = value
        siftUp(size)
        size++
    }

    fun addAll(collection: Collection<E>) {
        collection.forEach { add(it) }
    }

    fun removeMin(): E {
        if (size == 0) throw NoSuchElementException()

        val result = this[0]
        this[0] = this[size - 1]
        size--
        siftDown(0)
        return result
    }

    fun updatePriorityOf(value: E) {
        swap(0, indexByValue[value]!!)
        siftDown(0)
    }

    private tailrec fun siftUp(index: Int) {
        if (index.parent == -1) return
        if (this[index.parent] > this[index]) {
            swap(index.parent, index)
            siftUp(index.parent)
        }
    }

    private tailrec fun siftDown(index: Int) {
        var minIndex = index
        if (index.child1 < size && this[index.child1] < this[minIndex]) minIndex = index.child1
        if (index.child2 < size && this[index.child2] < this[minIndex]) minIndex = index.child2

        if (index != minIndex) {
            swap(index, minIndex)
            siftDown(minIndex)
        }
    }

    fun toSet(): Set<E> = 0.until(size).mapTo(LinkedHashSet()) { this[it] }

    fun toList(): List<E> = 0.until(size).mapTo(ArrayList()) { this[it] }

    override fun toString(): String {
        return toList().joinToString { it.toString() }
    }

    private operator fun E.compareTo(that: E) = comparator.compare(this, that)

    private fun swap(index1: Int, index2: Int) {
        val tmp = this[index1]
        this[index1] = this[index2]
        this[index2] = tmp
    }

    private operator fun get(index: Int): E {
        return array[index]!!
    }

    private operator fun set(index: Int, value: E?) {
        array[index] = value!!
        indexByValue[value] = index
    }

    fun isEmpty(): Boolean = size == 0

    fun isNotEmpty(): Boolean = !isEmpty()

    fun contains(value: E): Boolean = indexByValue.containsKey(value)

    companion object {
        fun <E> binaryHeapOf(vararg values: E): BinaryHeap<E> {
            val result = BinaryHeap<E>()
            values.forEach { result.add(it) }
            return result
        }

        private fun <E> defaultComparator(): Comparator<E> =
            Comparator { o1, o2 ->
                @Suppress("UNCHECKED_CAST")
                (o1 as Comparable<E>).compareTo(o2)
            }

        private val Int.child1: Int get() = (this * 2) + 1
        private val Int.child2: Int get() = (this * 2) + 2
        private val Int.parent: Int get() = if (this == 0) -1 else (this - 1) / 2
    }
}

class BinaryHeapTests {
    @Test fun `heap construction`() {
        binaryHeapOf(3, 2, 1).toSet() shouldEqual setOf(1, 2, 3)
    }

    @Test fun `keeps min value`() {
        val heap = BinaryHeap<Int>()
        10.downTo(1).forEach { n ->
            heap.add(n)
            heap.min() shouldEqual n
        }
    }

    @Test fun `removes min value`() {
        binaryHeapOf(1).removeMin() shouldEqual 1
        binaryHeapOf(3, 2, 1).let {
            it.removeMin() shouldEqual 1
            it.removeMin() shouldEqual 2
            it.removeMin() shouldEqual 3
        }
    }

    @Test fun `can grow beyond initial capacity`() {
        val heap = BinaryHeap<Int>(initialCapacity = 1)
        10.downTo(1).forEach { n ->
            heap.add(n)
        }
        heap.size() shouldEqual 10
    }

    @Test fun `cannot have duplicates`() {
        val heap = BinaryHeap<Int>()
        10.downTo(1).forEach {
            heap.add(42)
        }
        heap.size() shouldEqual 1
    }

    @Test fun `can use custom comparator`() {
        val map = mapOf("a" to 3, "b" to 1, "c" to 0)
        val customComparator = Comparator<String> { s1, s2 -> map[s1]!!.compareTo(map[s2]!!) }

        val heap = BinaryHeap(comparator = customComparator)
        heap.addAll(listOf("a", "b", "c"))

        heap.removeMin() shouldEqual "c"
        heap.removeMin() shouldEqual "b"
        heap.removeMin() shouldEqual "a"
    }

    @Test fun `can update priority of an item`() {
        val map = mutableMapOf("a" to 3, "b" to 1, "c" to 0)
        val customComparator = Comparator<String> { s1, s2 -> map[s1]!!.compareTo(map[s2]!!) }

        val heap = BinaryHeap(comparator = customComparator)
        heap.addAll(listOf("a", "b", "c"))

        heap.min() shouldEqual "c"
        map["a"] = -1
        heap.updatePriorityOf("a")
        heap.min() shouldEqual "a"
    }
}