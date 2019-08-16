package katas.kotlin.leetcode.min_number_of_chairs

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/356520
 */
class MinNumberOfChairsTests {
    @Test fun examples() {
        minNumberOfChairs(arrivedTime = arrayOf(1), leftTime = arrayOf(2)) shouldEqual 1
        minNumberOfChairs(arrivedTime = arrayOf(1, 2), leftTime = arrayOf(2, 3)) shouldEqual 1
        minNumberOfChairs(arrivedTime = arrayOf(1, 1), leftTime = arrayOf(2, 2)) shouldEqual 2
        minNumberOfChairs(arrivedTime = arrayOf(1, 1, 1), leftTime = arrayOf(5, 4, 3)) shouldEqual 3

        minNumberOfChairs(arrivedTime = arrayOf(1, 2, 6, 5, 3), leftTime = arrayOf(5, 5, 6, 7, 8)) shouldEqual 3
    }

    private fun minNumberOfChairs(arrivedTime: Array<Int>, leftTime: Array<Int>): Int {
        if (arrivedTime.size != leftTime.size) error("")
        arrivedTime.sort()
        leftTime.sort()

        var max = 0
        var i = 0
        var j = 0
        while (i < arrivedTime.size) {
            if (arrivedTime[i] < leftTime[j]) {
                i++
                max = maxOf(max, i - j)
            } else {
                j++
            }
        }
        return max
    }
}