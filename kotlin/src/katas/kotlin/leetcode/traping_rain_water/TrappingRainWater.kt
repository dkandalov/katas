package katas.kotlin.leetcode.traping_rain_water

import datsok.*
import org.junit.*
import java.util.*

/**
 * https://leetcode.com/problems/trapping-rain-water
 *
 * Given n non-negative integers representing an elevation map where the width of each bar is 1,
 * compute how much water it is able to trap after raining.
 */
class TrappingRainWater {
    private val trap = ::trap_with_lookback

    @Test fun `some examples`() {
        trap(listOf(0)) shouldEqual 0
        trap(listOf(1)) shouldEqual 0

        trap(listOf(1, 0)) shouldEqual 0
        trap(listOf(0, 1)) shouldEqual 0
        trap(listOf(1, 1)) shouldEqual 0

        trap(listOf(0, 1, 1)) shouldEqual 0
        trap(listOf(1, 1, 0)) shouldEqual 0
        trap(listOf(1, 0, 1)) shouldEqual 1

        trap(listOf(1, 2, 0)) shouldEqual 0
        trap(listOf(0, 2, 1)) shouldEqual 0
        trap(listOf(1, 0, 2)) shouldEqual 1
        trap(listOf(2, 0, 1)) shouldEqual 1

        trap(listOf(2, 0, 2)) shouldEqual 2
        trap(listOf(2, 0, 0, 1)) shouldEqual 2
        trap(listOf(1, 0, 0, 2)) shouldEqual 2
        trap(listOf(2, 0, 0, 2)) shouldEqual 4

        trap(listOf(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) shouldEqual 6
    }
}

private fun trap_with_lookback(elevationMap: List<Int>): Int {
    val prevIndexByHeight = TreeMap<Int, Int>()
    return elevationMap.indices.zip(elevationMap).sumBy { (index, wallHeight) ->
        (1..wallHeight).sumBy { height ->
            val prevIndex = prevIndexByHeight.ceilingEntry(height)?.value ?: index - 1
            (index - prevIndex - 1).also {
                prevIndexByHeight[height] = index
            }
        }
    }
}

private fun trap_with_lookahead(elevationMap: List<Int>): Int {
    fun volumeTillNextWall(fromIndex: Int, height: Int): Int {
        val volume = elevationMap.subList(fromIndex + 1, elevationMap.size).takeWhile { it < height }.size
        val doesNotSpillFromRight = volume < elevationMap.size - (fromIndex + 1)
        return if (doesNotSpillFromRight) volume else 0
    }

    return elevationMap.indices.zip(elevationMap).sumBy { (index, wallHeight) ->
        (1..wallHeight).sumBy { height ->
            volumeTillNextWall(index, height)
        }
    }
}
