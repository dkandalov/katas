package katas.kotlin.leetcode.traping_rain_water

import org.hamcrest.CoreMatchers.*
import org.junit.*
import org.junit.Assert.*

/**
 * https://leetcode.com/problems/trapping-rain-water
 */
class TrappingRainWater {
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
        assertThat(trap(listOf(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)), equalTo(6))
    }
}

private fun trap(map: List<Int>): Int {
    fun sumVolumeAhead(index: Int, height: Int): Int {
        val volumeTillNextWall = (index + 1).until(map.size).takeWhile { map[it] < height }.size
        val doesNotSpillFromRight = volumeTillNextWall < map.size - (index + 1)
        return if (doesNotSpillFromRight) volumeTillNextWall else 0
    }

    var sum = 0
    map.forEachIndexed { index, wallHeight ->
        (1..wallHeight).forEach { height ->
            sum += sumVolumeAhead(index, height)
        }
    }
    return sum
}
