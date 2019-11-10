package katas.kotlin.leetcode.traping_rain_water

import org.hamcrest.CoreMatchers.*
import org.junit.*
import org.junit.Assert.*

/**
 * https://leetcode.com/problems/trapping-rain-water
 */
class TrappingRainWater {
    @Test fun `some examples`() {
        assertThat(trap(listOf(1, 0, 2)), equalTo(1))
        //assertThat(trap(listOf(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)), equalTo(6))
    }
}

private fun trap(map: List<Int>): Int {
    var sum = 0
    map.forEachIndexed { index, value ->
        (1..value).forEach { height ->
            sum += (index + 1).until(map.size).takeWhile { map[it] <= height }.size
        }
    }
    return 1
}
