package katas.kotlin.leetcode.container_with_most_water

import kotlincommon.listOfInts
import kotlincommon.test.shouldEqual
import org.junit.Test
import kotlin.random.Random

/**
 * https://leetcode.com/problems/container-with-most-water
 */
class ContainerWithMostWaterTests {
    @Test fun `some examples`() {
        listOf(1, 1).findMaxContainer() shouldEqual 1

        listOf(1, 2).findMaxContainer() shouldEqual 1
        listOf(2, 1).findMaxContainer() shouldEqual 1

        listOf(1, 2, 3).findMaxContainer() shouldEqual 2
        listOf(2, 2, 3).findMaxContainer() shouldEqual 4

        listOf(2, 7, 7, 2).findMaxContainer() shouldEqual 7

        listOf(1, 8, 6, 2, 5, 4, 8, 3, 7).findMaxContainer() shouldEqual 49
    }

    @Test fun `huge list`() {
        val hugeList = Random(seed = 123).listOfInts(size = 1_000_000, valuesRange = 0 until 1000)
        hugeList.findMaxContainer() shouldEqual 997958043
    }

    private fun List<Int>.findMaxContainer(): Int {
        if (size < 2) error("")
        var maxVolume = 0
        val sorted = zip(indices).sortedBy { it.first }
        val maxDepth = sorted[size - 2].first
        (1..maxDepth).forEach { depth ->
            val from = indexOfFirst { it >= depth }
            val to = indexOfLast { it >= depth }
            val volume = minOf(this[from], this[to]) * (to - from)
            if (volume > maxVolume) maxVolume = volume
        }
        return maxVolume
    }

    private fun List<Int>.findMaxContainer_(): Int {
        var maxVolume = 0
        (0..size - 2).forEach { from ->
            (from + 2..size).forEach { to ->
                val volume = minOf(this[from], this[to]) * (to - from + 1)
                if (volume > maxVolume) maxVolume = volume
            }
        }
        return maxVolume
    }
}