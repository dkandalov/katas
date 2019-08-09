package katas.kotlin.leetcode.median_of_twoarrays

import kotlincommon.test.shouldEqual
import org.junit.Test

class Median {
    @Test fun `find median of two sorted arrays`() {
        median(arrayOf(1, 3), arrayOf(2)) shouldEqual 2.0
        median(arrayOf(1, 2), arrayOf(3, 4)) shouldEqual 2.5
    }

    private fun median(array1: Array<Int>, array2: Array<Int>): Double {
        val ints = array1 + array2
        ints.sort()
        val midIndex = ints.size / 2
        return if (ints.size % 2 == 0) (ints[midIndex - 1] + ints[midIndex]) / 2.0
        else ints[midIndex].toDouble()
    }
}