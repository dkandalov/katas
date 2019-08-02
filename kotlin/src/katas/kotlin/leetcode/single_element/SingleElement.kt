package katas.kotlin.leetcode.single_element

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/346626/google-phone-screen-single-element
 */
class SingleElementTests {
    @Test fun `find single element that appears alone in an array of pairs`() {
        arrayOf(1, 2, 2).findSingleElement() shouldEqual 1
        arrayOf(2, 1, 2).findSingleElement() shouldEqual 1
        arrayOf(2, 2, 1).findSingleElement() shouldEqual 1
    }
}

private fun <T> Array<T>.findSingleElement(): Int {
    return 1
}
