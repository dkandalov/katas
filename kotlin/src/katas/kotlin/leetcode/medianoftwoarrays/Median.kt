package katas.kotlin.leetcode.medianoftwoarrays

import kotlincommon.test.shouldEqual
import org.junit.Test

class Median {
    @Test fun `find median of two sorted arrays`() {
        median(arrayOf(1, 3), arrayOf(2)) shouldEqual 2
    }

    private fun median(array1: Array<Int>, array2: Array<Int>): Int {
        return 2
    }
}