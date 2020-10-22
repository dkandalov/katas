package katas.kotlin.leetcode.subarray_sum_equals_k

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
// 5 7 10  6  5 6
// f            t

fun subarraySum(ints: IntArray, targetSum: Int): Int {
    return subArrays(Ints(ints.toList()), targetSum).count()
}

private fun subArrays(ints: Ints, targetSum: Int): Sequence<Pair<Int, Int>> = sequence {
    (0..ints.lastIndex).forEach { from ->
        var sum = 0
        (from..ints.lastIndex).forEach { to ->
            sum += ints[to]
            if (sum == targetSum) yield(Pair(from, to))
        }
    }
}

class Ints(value: List<Int>) : List<Int> by value
