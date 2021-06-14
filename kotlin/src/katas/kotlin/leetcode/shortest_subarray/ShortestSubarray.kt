package katas.kotlin.leetcode.shortest_subarray

import datsok.shouldEqual
import org.junit.jupiter.api.Test
import java.io.File
import kotlin.collections.ArrayDeque
import kotlin.collections.component1
import kotlin.collections.component2
import kotlin.collections.forEach
import kotlin.collections.indices
import kotlin.collections.isNotEmpty
import kotlin.collections.lastIndex
import kotlin.collections.map
import kotlin.collections.sliceArray
import kotlin.collections.sum
import kotlin.collections.toIntArray


// https://leetcode.com/problems/shortest-subarray-with-sum-at-least-k
//
// Return the length of the shortest, non-empty, contiguous subarray of A with sum at least K.
// If there is no non-empty subarray with sum at least K, return -1.
//
// Example 1:
// Input: A = [1], K = 1
// Output: 1
//
// Example 2:
// Input: A = [1,2], K = 4
// Output: -1
//
// Example 3:
// Input: A = [2,-1,2], K = 3
// Output: 3

// [10,  5, 8, 9, 11] <- prefix sum
// [10, -5, 3, 1, 2]
//   f            t

// [10,  -5, -2, -1, 1] <- prefix sum
// [10, -15,  3,  1, 2]
//   f               t

fun shortestSubarray_(ints: IntArray, minSum: Int): Int {
    return ints.findShortestSubarray_bruteForce(minSum = minSum)?.size ?: -1
}

fun IntArray.findShortestSubarray_bruteForce(minSum: Int): IntArray? {
    val subArrays = sequence {
        (0..lastIndex).forEach { i1 ->
            (i1..lastIndex).forEach { i2 ->
                yield(sliceArray(i1..i2))
            }
        }
    }
    return subArrays
        .filter { it.sum() >= minSum }
        .minByOrNull { it.size }
}

class MonoQueue<T : Comparable<T>> {
    private val deque = ArrayDeque<T>()

    fun addLast(element: T) {
        while (deque.isNotEmpty() && element <= deque.last()) {
            deque.removeLast()
        }
        deque.addLast(element)
    }

    fun removeFirstWhile(f: (T) -> Boolean): List<T> {
        val removed = ArrayList<T>()
        while (deque.isNotEmpty() && f(deque.first())) {
            removed.add(deque.removeFirst())
        }
        return removed
    }
}

fun shortestSubarray(ints: IntArray, minSum: Int): Int {
    data class Entry(val index: Int, val prefixSum: Long): Comparable<Entry> {
        override fun compareTo(other: Entry) = prefixSum.compareTo(other.prefixSum)
    }

    val monoq = MonoQueue<Entry>()
    val candidates = ints.prefixSum().flatMapIndexed { index, prefixSum ->
        monoq.addLast(Entry(index, prefixSum))
        monoq.removeFirstWhile { prefixSum - it.prefixSum >= minSum }
            .map { index - it.index }
    }
    return candidates.minOrNull() ?: -1
}

private fun IntArray.prefixSum(): LongArray {
    val result = LongArray(size + 1)
    indices.forEach { i ->
        result[i + 1] = result[i] + this[i]
    }
    return result
}

fun shortestSubarray__(ints: IntArray, minSum: Int): Int {
    val prefixSum = ints.prefixSum()

    val minSize = sequence {
        val monoq = ArrayDeque<Int>()
        prefixSum.indices.forEach { toIndex ->
            while (monoq.isNotEmpty() && prefixSum[monoq.last()] >= prefixSum[toIndex]) {
                monoq.removeLast()
            }
            monoq.addLast(toIndex)

            while (monoq.isNotEmpty() && prefixSum[toIndex] - prefixSum[monoq.first()] >= minSum) {
                val fromIndex = monoq.removeFirst()
                yield(fromIndex..toIndex)
            }
        }
    }.minOfOrNull { it.last - it.first }

    return minSize ?: -1
}


class Examples {
    @Test fun `some examples`() {
        shortestSubarray(intArrayOf(1), minSum = 1) shouldEqual 1
        shortestSubarray(intArrayOf(1, 2), minSum = 4) shouldEqual -1
        shortestSubarray(intArrayOf(2, -1, 2), minSum = 3) shouldEqual 3
        shortestSubarray(intArrayOf(2, -1, 3, 5), minSum = 3) shouldEqual 1
        shortestSubarray(intArrayOf(2, -1, 3, 9), minSum = 10) shouldEqual 2
    }

    @Test fun `large input`() {
        val (intArray, k) = File("src/katas/kotlin/leetcode/shortest_subarray/large-testcase.txt")
            .readLines().let { (inputLine, minSumLine) ->
                Pair(
                    inputLine.drop(1).dropLast(1).split(",").map { it.toInt() }.toIntArray(),
                    minSumLine.toInt()
                )
            }
        shortestSubarray(intArray, k) shouldEqual 25813
    }
}
