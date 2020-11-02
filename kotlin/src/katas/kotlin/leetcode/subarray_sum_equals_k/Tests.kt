package katas.kotlin.leetcode.subarray_sum_equals_k

import java.util.*

//
// https://leetcode.com/problems/subarray-sum-equals-k
//
// Given an array of integers and an integer k, you need to find the total number of continuous subarrays whose sum equals to k.
// Constraints:
//    - The length of the array is in range [1, 20000].
//    - The range of numbers in the array is [-1000, 1000] and the range of the integer k is [-1e7, 1e7].
//
// Example 1:
// Input:nums = [1,1,1], k = 2
// Output: 2
//

// 5 2  3 -4 -1 1
// 5 7 10  6  5 6 <- prefix sum
// f            t

fun subarraySum(ints: IntArray, targetSum: Int): Int {
    return subArrays(ints, targetSum).count()
}

private fun subArrays(ints: IntArray, targetSum: Int): Sequence<Pair<Int, Int>> = sequence {
    (0..ints.lastIndex).forEach { from ->
        var sum = 0
        (from..ints.lastIndex).forEach { to ->
            sum += ints[to]
            if (sum == targetSum) yield(Pair(from, to))
        }
    }
}


fun subarraySum_cleverHashMap(nums: IntArray, targetSum: Int): Int {
    var count = 0
    val countBySum = HashMap<Int, Int>()
    countBySum[0] = 1
    var sum = 0
    nums.forEach {
        sum += it
        count += countBySum.getOrDefault(sum - targetSum, defaultValue = 0)
        countBySum[sum] = countBySum.getOrDefault(sum, defaultValue = 0) + 1
    }
    return count
}