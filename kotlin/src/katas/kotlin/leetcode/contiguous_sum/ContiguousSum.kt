package katas.kotlin.leetcode.contiguous_sum

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * Given an array of integers and a target, check if there is a contiguous sequence that sums to that target.
 */
class ContiguousSumTests {
    @Test fun examples() {
        findRange(arrayOf(2, 5, 7), target = 2) shouldEqual IntRange(0, 0)
        findRange(arrayOf(2, 5, 7), target = 5) shouldEqual IntRange(1, 1)
        findRange(arrayOf(2, 5, 7), target = 7) shouldEqual IntRange(0, 1)
        findRange(arrayOf(2, 5, 7), target = 12) shouldEqual IntRange(1, 2)
        findRange(arrayOf(2, 5, 7), target = 14) shouldEqual IntRange(0, 2)
        findRange(arrayOf(2, 5, 7), target = 100) shouldEqual null

        findRange(arrayOf(-2, 5, -2, -3, -8, 1), target = -12) shouldEqual IntRange(2, 5)
    }
}

private fun findRange(array: Array<Int>, target: Int): IntRange? {
    var sum = 0
    val sums = array.map { n ->
        sum += n
        sum
    }
    (0 until array.size).forEach { from ->
        (from until array.size).forEach { to ->
            val rangeSum = sums[to] - (if (from > 0) sums[from - 1] else 0)
            if (rangeSum == target) return from..to
        }
    }
    return null
}

private fun findRange_(array: Array<Int>, target: Int): IntRange? {
    (0 until array.size).forEach { from ->
        (from until array.size).forEach { to ->
            val range = from..to
            if (range.sumBy { array[it] } == target) return range
        }
    }
    return null
}
