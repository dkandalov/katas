package katas.kotlin.leetcode.container_with_most_water

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/container-with-most-water
 */
class ContainerWithMostWaterTests {
    @Test fun `example`() {
        listOf(1, 8, 6, 2, 5, 4, 8, 3, 7).findMaxContainer() shouldEqual 49
    }

    private fun List<Int>.findMaxContainer(): Int {
        var maxVolume = 0
        (0..size - 2).forEach { from ->
            (from + 2..size).forEach { to ->
                val subset = subList(from, to)
                val volume = minOf(subset.first(), subset.last()) * (subset.size - 1)
                if (volume > maxVolume) maxVolume = volume
            }
        }
        return maxVolume
    }
}