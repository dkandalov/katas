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
    slidingWindow { n1, n2 -> if (n1 != n2) return n1 }
    error("")
}

private inline fun Array<Int>.slidingWindow(f: (Int, Int?) -> Unit) {
    var i1 = 0
    while (i1 < size) {
        val i2 = i1 + 1
        f(this[i1], if (i2 == size) null else this[i2])
        i1 += 2
    }
}
