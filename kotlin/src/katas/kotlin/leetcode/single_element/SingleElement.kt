package katas.kotlin.leetcode.single_element

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/346626/google-phone-screen-single-element
 */
class SingleElementTests {
//    private fun Array<Int>.findSingleElement() = this.slidingFind()
    private fun Array<Int>.findSingleElement() = binaryFind(this)

    @Test fun `find single element that appears alone in an array of pairs`() {
        arrayOf(1, 2, 2).findSingleElement() shouldEqual 1
        arrayOf(2, 2, 1, 2, 2).findSingleElement() shouldEqual 1
        arrayOf(2, 2, 1).findSingleElement() shouldEqual 1

        arrayOf(2, 2, 1, 1, 9, 9, 5, 2, 2).findSingleElement() shouldEqual 5
    }
}

private fun binaryFind(a: Array<Int>): Int {
    if (a[a.size - 1] != a[a.size - 2]) return a[a.size - 1]
    val size = a.size / 2
    var from = 0
    var to = size - 1
    while (true) {
        val mid = (from + to) / 2
        when {
            a.isDiffAt(from) -> return a.firstAt(from)
            a.isDiffAt(mid)  -> to = mid
            else             -> from = mid + 1
        }
    }
}

private fun Array<Int>.firstAt(index: Int) = this[index * 2]
private fun Array<Int>.secondAt(index: Int) = this[index * 2 + 1]
private fun Array<Int>.isDiffAt(index: Int) = firstAt(index) != secondAt(index)

private fun Array<Int>.slidingFind(): Int {
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
