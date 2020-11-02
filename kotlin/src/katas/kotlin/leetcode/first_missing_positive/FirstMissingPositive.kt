package katas.kotlin.leetcode.first_missing_positive

import datsok.shouldEqual
import org.junit.jupiter.api.Test

//
// https://leetcode.com/problems/first-missing-positive
//
// Given an unsorted integer array, find the smallest missing positive integer.
//
// Example 1:
// Input: [1,2,0]
// Output: 3
//
// Example 2:
// Input: [3,4,-1,1]
// Output: 2
//
// Example 3:
// Input: [7,8,9,11,12]
// Output: 1
//
// Follow up:
// Your algorithm should run in O(n) time and uses constant extra space.
//


fun firstMissingPositive(nums: IntArray): Int {
    nums.indices.filter { nums[it] == 0 }.forEach { nums[it] = -1 }
    nums.filter { it - 1 in nums.indices }.forEach { nums[it - 1] = 0 }
    return (nums.indices.find { nums[it] != 0 } ?: nums.size) + 1
}

fun firstMissingPositive_2(nums: IntArray): Int {
    val numbers = nums.sorted().filter { it > 0 }.toSet()
    numbers.forEachIndexed { i, n ->
        if (i + 1 != n) return i + 1
    }
    return (numbers.lastOrNull() ?: 0) + 1
}

fun firstMissingPositive_1(nums: IntArray): Int {
    val positiveNumbers = nums.filter { it > 0 }
    val max = positiveNumbers.maxOrNull() ?: 0
    return 1.until(max).find { it !in positiveNumbers } ?: max + 1
}

fun firstMissingPositive_0(nums: IntArray): Int {
    return (1..Int.MAX_VALUE).find { it !in nums } ?: -1
}

class FirstMissingPositiveTests {
    private val f: (IntArray) -> Int = ::firstMissingPositive

    @Test fun `some examples`() {
        f(intArrayOf()) shouldEqual 1
        f(intArrayOf(-1)) shouldEqual 1
        f(intArrayOf(0)) shouldEqual 1
        f(intArrayOf(123)) shouldEqual 1
        f(intArrayOf(Int.MAX_VALUE)) shouldEqual 1

        f(intArrayOf(1)) shouldEqual 2
        f(intArrayOf(1, 2, 0)) shouldEqual 3
        f(intArrayOf(1, 1, 2, 2, 0)) shouldEqual 3
        f(intArrayOf(3, 4, -1, 1)) shouldEqual 2
        f(intArrayOf(7, 8, 9, 11, 12)) shouldEqual 1
    }
}