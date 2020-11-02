package katas.kotlin.leetcode.shortest_subarray

import datsok.shouldEqual
import org.junit.jupiter.api.Test
import java.io.File


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

fun shortestSubarray_(ints: IntArray, targetSum: Int): Int {
    return ints.findShortestSubarray_bruteForce(targetSum = targetSum)?.size ?: -1
}

fun IntArray.findShortestSubarray_bruteForce(targetSum: Int): IntArray? {
    val subArrays = sequence {
        (0..lastIndex).forEach { i1 ->
            (i1..lastIndex).forEach { i2 ->
                yield(sliceArray(i1..i2))
            }
        }
    }
    return subArrays
        .filter { it.sum() >= targetSum }
        .minByOrNull { it.size }
}

fun shortestSubarray(ints: IntArray, targetSum: Int): Int {
    val prefixSum = LongArray(ints.size + 1)
    ints.indices.forEach { i ->
        prefixSum[i + 1] = prefixSum[i] + ints[i]
    }

    val minSize = sequence {
        val monoq = ArrayDeque<Int>()
        prefixSum.indices.forEach { toIndex ->
            while (monoq.isNotEmpty() && prefixSum[monoq.last()] >= prefixSum[toIndex]) {
                monoq.removeLast()
            }
            monoq.addLast(toIndex)

            while (monoq.isNotEmpty() && prefixSum[toIndex] - prefixSum[monoq.first()] >= targetSum) {
                val fromIndex = monoq.removeFirst()
                yield(fromIndex..toIndex)
            }
        }
    }.minOfOrNull { it.last - it.first }

    return minSize ?: -1
}


class Examples {
    @Test fun `some examples`() {
        shortestSubarray(intArrayOf(1), targetSum = 1) shouldEqual 1
        shortestSubarray(intArrayOf(1, 2), targetSum = 4) shouldEqual -1
        shortestSubarray(intArrayOf(2, -1, 2), targetSum = 3) shouldEqual 3
        shortestSubarray(intArrayOf(2, -1, 3, 5), targetSum = 3) shouldEqual 1
        shortestSubarray(intArrayOf(2, -1, 3, 9), targetSum = 10) shouldEqual 2
    }

    @Test fun `large input`() {
        val (intArray, k) = File("src/katas/kotlin/leetcode/shortest_subarray/large-testcase.txt")
            .readLines().let { (inputLine, targetSumLine) ->
                Pair(
                    inputLine.drop(1).dropLast(1).split(",").map { it.toInt() }.toIntArray(),
                    targetSumLine.toInt()
                )
            }
        shortestSubarray(intArray, k) shouldEqual 25813
    }
}
