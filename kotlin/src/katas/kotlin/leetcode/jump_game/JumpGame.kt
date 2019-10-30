package katas.kotlin.leetcode.jump_game

import kotlincommon.test.shouldEqual
import org.junit.Test

class JumpGame {
    @Test fun `some examples`() {
        jump() shouldEqual emptyList()
        jump(1) shouldEqual emptyList()

        jump(1, 1) shouldEqual listOf(1)
        jump(1, 1, 1) shouldEqual listOf(1, 1)
        jump(2, 1, 1) shouldEqual listOf(2)

        jump(2, 3, 1, 1, 4) shouldEqual listOf(1, 3)
        jump(10, 3, 1, 1, 4) shouldEqual listOf(4)
    }

    private fun jump(vararg nums: Int): List<Int> = jump(nums.toTypedArray())

    private fun jump(nums: Array<Int>): List<Int> {
        if (nums.size <= 1) return emptyList()
        val jumps = ArrayList<Int>()

        var i = 0
        var lastRangeEnd = 1
        while (true) {
            val range = lastRangeEnd..(i + nums[i])
            lastRangeEnd = range.last
            if (nums.lastIndex in range) break

            val max = range.maxBy { it + nums[it] }!!
            jumps.add(max - i)
            i = max
        }

        jumps.add(nums.lastIndex - i)
        return jumps
    }

    private fun allJumps(nums: List<Int>, i: Int = 0): List<List<Int>> {
        if (i >= nums.size - 1) return listOf(emptyList())
        return (1..nums[i]).flatMap { jumpSize ->
            allJumps(nums, i + jumpSize).map { listOf(jumpSize) + it }
        }
    }
}