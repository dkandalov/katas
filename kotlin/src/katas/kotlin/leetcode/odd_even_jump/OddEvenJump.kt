package katas.kotlin.leetcode.odd_even_jump

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * ✅ https://leetcode.com/problems/odd-even-jump/
 */
class OddEvenJumpTests {
    @Test fun `find number of starting indices that can reach the end of array`() {
        oddEvenJumps(intArrayOf()) shouldEqual 0
        oddEvenJumps(intArrayOf(1)) shouldEqual 1

        oddEvenJumps(intArrayOf(1, 1)) shouldEqual 2
        oddEvenJumps(intArrayOf(1, 2)) shouldEqual 2
        oddEvenJumps(intArrayOf(1, 0)) shouldEqual 1
        oddEvenJumps(intArrayOf(1, 0, 1)) shouldEqual 3
        oddEvenJumps(intArrayOf(1, 0, 0)) shouldEqual 2

        oddEvenJumps(intArrayOf(10, 13, 12, 14, 15)) shouldEqual 2
        oddEvenJumps(intArrayOf(2, 3, 1, 1, 4)) shouldEqual 3
        oddEvenJumps(intArrayOf(5, 1, 3, 4, 2)) shouldEqual 3
    }
}

private fun oddEvenJumps(a: IntArray): Int {
    return a.indices.count { canReachEnd(it, a) }
}

private fun canReachEnd(index: Int, a: IntArray): Boolean {
    var oddJump = true
    var i = index
    while (i < a.size - 1) {
        val j = if (oddJump) doOddJump(i, a) else doEvenJump(i, a)
        if (i == j) return false
        i = j
        oddJump = !oddJump
    }
    return true
}

private fun doOddJump(i: Int, a: IntArray): Int {
    var j = i + 1
    var min = Int.MAX_VALUE
    var minIndex = i
    while (j < a.size) {
        if (a[i] <= a[j] && a[j] < min) {
            min = a[j]
            minIndex = j
        }
        j++
    }
    return minIndex
}

private fun doEvenJump(i: Int, a: IntArray): Int {
    var j = i + 1
    var max = Int.MIN_VALUE
    var maxIndex = i
    while (j < a.size) {
        if (a[i] >= a[j] && a[j] > max) {
            max = a[j]
            maxIndex = j
        }
        j++
    }
    return maxIndex
}
