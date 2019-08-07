package katas.kotlin.leetcode.odd_even_jump

import kotlincommon.test.shouldEqual
import org.junit.Test

class OddEvenJumpTests {
    @Test fun `find number of starting indices that can reach the end of array`() {
        oddEvenJumps(intArrayOf()) shouldEqual 0
        oddEvenJumps(intArrayOf(1)) shouldEqual 1
    }
}

private fun oddEvenJumps(a: IntArray): Int {
    return a.indices.count { canReachEnd(it, a) }
}

fun canReachEnd(i: Int, a: IntArray): Boolean {
    return true
}
