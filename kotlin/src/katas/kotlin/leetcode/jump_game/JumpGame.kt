package katas.kotlin.leetcode.jump_game

import kotlincommon.test.shouldEqual
import org.junit.Test

class JumpGame {
    @Test fun `some examples`() {
        jump(emptyArray()) shouldEqual emptyList()
        jump(arrayOf(2, 3, 1, 1, 4)) shouldEqual listOf(1, 3)
        jump(arrayOf(10, 3, 1, 1, 4)) shouldEqual listOf(4)
    }

    private fun jump(nums: Array<Int>): List<Int> {
        return allJumps(nums.toList()).minBy { it.size }!!
    }

    private fun allJumps(nums: List<Int>, i: Int = 0): List<List<Int>> {
        if (i >= nums.size - 1) return listOf(emptyList())
        return (1..nums[i]).flatMap { jumpSize ->
            allJumps(nums, i + jumpSize).map { listOf(jumpSize) + it }
        }
    }
}