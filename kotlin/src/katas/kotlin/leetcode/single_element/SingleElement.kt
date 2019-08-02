package katas.kotlin.leetcode.single_element

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/346626/google-phone-screen-single-element
 */
class SingleElementTests {
    @Test fun `find single element that appears alone in an array of pairs`() {
        arrayOf(1, 2, 2).findSingleElement() shouldEqual 1
        arrayOf(2, 2, 1, 2, 2).findSingleElement() shouldEqual 1
        arrayOf(2, 2, 1).findSingleElement() shouldEqual 1

        arrayOf(2, 2, 1, 1, 9, 9, 5, 2, 2).findSingleElement() shouldEqual 5
    }
}

private fun Array<Int>.findSingleElement(): Int {
    asIterable().windowed(size = 2, step = 2, partialWindows = true).forEach {
        val n1 = it[0]
        val n2 = if (it.size > 1) it[1] else null
        if (n1 != n2) return n1
    }
    error("")
}
