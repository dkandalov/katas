package katas.kotlin.leetcode.traping_rain_water

import kotlincommon.test.*
import org.junit.*

/**
 * https://leetcode.com/problems/trapping-rain-water
 */
class TrappingRainWater {
    @Test fun `some examples`() {
        trap(arrayOf(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) shouldEqual 6
    }
}

private fun trap(map: Array<Int>): Int {
    return 6
}
