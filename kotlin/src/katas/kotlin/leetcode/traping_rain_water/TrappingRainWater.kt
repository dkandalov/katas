package katas.kotlin.leetcode.traping_rain_water

import org.hamcrest.CoreMatchers.*
import org.junit.*
import org.junit.Assert.*

/**
 * https://leetcode.com/problems/trapping-rain-water
 *
 * Given n non-negative integers representing an elevation map where the width of each bar is 1,
 * compute how much water it is able to trap after raining.
 */
class TrappingRainWater {
    private val trap = ::trap_1

    @Test fun `some examples`() {
        assertThat(trap(listOf(0)), equalTo(0))
        assertThat(trap(listOf(1)), equalTo(0))

        assertThat(trap(listOf(1, 0)), equalTo(0))
        assertThat(trap(listOf(0, 1)), equalTo(0))
        assertThat(trap(listOf(1, 1)), equalTo(0))

        assertThat(trap(listOf(0, 1, 1)), equalTo(0))
        assertThat(trap(listOf(1, 1, 0)), equalTo(0))
        assertThat(trap(listOf(1, 0, 1)), equalTo(1))

        assertThat(trap(listOf(1, 2, 0)), equalTo(0))
        assertThat(trap(listOf(0, 2, 1)), equalTo(0))
        assertThat(trap(listOf(1, 0, 2)), equalTo(1))
        assertThat(trap(listOf(2, 0, 1)), equalTo(1))

        assertThat(trap(listOf(2, 0, 2)), equalTo(2))
        assertThat(trap(listOf(2, 0, 0, 1)), equalTo(2))
        assertThat(trap(listOf(1, 0, 0, 2)), equalTo(2))
        assertThat(trap(listOf(2, 0, 0, 2)), equalTo(4))

        assertThat(trap(listOf(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)), equalTo(6))
    }
}

private fun trap_1(elevationMap: List<Int>): Int {
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
