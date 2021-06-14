package katas.kotlin.leetcode.jump_game_ii

import datsok.shouldEqual
import org.junit.jupiter.api.Test

//
// https://leetcode.com/problems/jump-game-ii ✅
//
// Given an array of non-negative integers, you are initially positioned at the first index of the array.
// Each element in the array represents your maximum jump length at that position.
// Your goal is to reach the last index in the minimum number of jumps.
// Note: You can assume that you can always reach the last index.
// Constraints:
//    1 <= nums.length <= 3 * 10^4
//    0 <= nums[i] <= 10^5
//
// Example:
// Input: [2,3,1,1,4]
// Output: 2
// Explanation: The minimum number of jumps to reach the last index is 2.
//              Jump 1 step from index 0 to 1, then 3 steps to the last index.
//
// Example 2:
// Input: nums = [2,3,0,1,4]
// Output: 2
//

fun jump(nums: IntArray): Int {
    return if (nums.size <= 1) 0
    else findMinJumps(nums.toList()).size
}

private fun findMinJumps(nums: List<Int>): List<Int> =
    generateSequence(seed = 0) { i ->
        if (i + nums[i] >= nums.lastIndex) null
        else {
            val nextIndices = (i + 1)..(i + nums[i])
            nextIndices.maxByOrNull { it: Int -> it + nums[it] }!!
        }
    }.toList()

fun jump_(nums: IntArray): Int {
    require(nums.all { it > 0 })
    if (nums.size <= 1) return 0

    var jumpCount = 1
    var reachableIndices = 1..nums[0]
    while (nums.lastIndex !in reachableIndices) {
        var maxSteps = Int.MIN_VALUE
        reachableIndices.forEach { index ->
            val steps = nums[index]
            if (steps >= maxSteps) {
                maxSteps = steps
                reachableIndices = (index + 1)..(index + steps)
            }
        }
        jumpCount++
    }
    return jumpCount
}

class JumpGameTests {
    @Test fun `some examples`() {
        findMinJumps(listOf(0)) shouldEqual listOf(0)
        findMinJumps(listOf(1)) shouldEqual listOf(0)
        findMinJumps(listOf(1, 1, 1)) shouldEqual listOf(0, 1)
        findMinJumps(listOf(2, 3, 1, 1, 4)) shouldEqual listOf(0, 1)
        findMinJumps(listOf(2, 3, 0, 1, 4)) shouldEqual listOf(0, 1)
        findMinJumps(listOf(2, 3, 3, 1, 4)) shouldEqual listOf(0, 2)
        findMinJumps(listOf(2, 3, 3, 1, 4, 0)) shouldEqual listOf(0, 2)
        findMinJumps(listOf(2, 3, 3, 1, 4, 0, 0)) shouldEqual listOf(0, 2, 4)
    }
}
