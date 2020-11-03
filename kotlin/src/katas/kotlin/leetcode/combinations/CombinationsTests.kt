package katas.kotlin.leetcode.combinations

import datsok.shouldEqual
import org.junit.jupiter.api.Test

//
// https://leetcode.com/problems/combinations ✅
//
// Given two integers n and k, return all possible combinations of k numbers out of 1 ... n.
// You may return the answer in any order.
// Constraints:
//     1 <= n <= 20
//     1 <= k <= n
//
// Example 1:
// Input: n = 4, k = 2
// Output:
// [
//   [2,4],
//   [3,4],
//   [2,3],
//   [1,2],
//   [1,3],
//   [1,4],
// ]
//
// Example 2:
// Input: n = 1, k = 1
// Output: [[1]]
//

fun combine(n: Int, k: Int): List<List<Int>> {
    require(n in 1..20 && k in 1..n)
    return subsetsOf(1..n, maxSize = k)
}

private fun subsetsOf(range: IntRange, maxSize: Int): List<List<Int>> {
    if (maxSize == 0) return listOf(emptyList())
    if (range.size == maxSize) return listOf(range.toList())

    return subsetsOf(range.shiftStart(), maxSize) +
        subsetsOf(range.shiftStart(), maxSize - 1).map { listOf(range.first) + it }
}

private fun IntRange.shiftStart() = IntRange(start + 1, endInclusive)
private val IntRange.size: Int get() = endInclusive + 1 - start

class CombinationsTests {
    @Test fun `some examples`() {
        combine(n = 1, k = 1) shouldEqual listOf(listOf(1))
        combine(n = 2, k = 1) shouldEqual listOf(listOf(2), listOf(1))
        combine(n = 2, k = 2) shouldEqual listOf(listOf(1, 2))
        combine(n = 4, k = 2) shouldEqual listOf(
            listOf(3, 4),
            listOf(2, 4),
            listOf(2, 3),
            listOf(1, 4),
            listOf(1, 3),
            listOf(1, 2)
        )
    }
}