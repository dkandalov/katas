package katas.kotlin.leetcode.whac_a_mole

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/350139/Google-or-Phone-Screen-or-Whac-A-Mole
 */
class WhacMoleTests {
    @Test fun `hit max amount of moles`() {
        hit(arrayOf(0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0), malletWidth = 4) shouldEqual Pair(1, 3)
        hit(arrayOf(0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0), malletWidth = 5) shouldEqual Pair(1, 4)
        hit(arrayOf(0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0), malletWidth = 6) shouldEqual Pair(0, 4)
    }

    @Test fun `hit max amount of moles with two mallets`() {
        hit2(arrayOf(0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0), malledWidth = 5) shouldEqual Triple(1, 7, 6)
    }
}

private fun hit(holes: Array<Int>, malletWidth: Int): Pair<Int, Int> {
    var max = -1
    var maxIndex = -1
    holes.toList().windowed(size = malletWidth, step = 1).forEachIndexed { i, window ->
        val sum = window.sum()
        if (sum > max) {
            max = sum
            maxIndex = i
        }
    }
    return Pair(maxIndex, max)
}

private fun hit2(holes: Array<Int>, malledWidth: Int): Triple<Int, Int, Int> {
    var max = -1
    var maxIndex1 = -1
    var maxIndex2 = -1
    holes.toList().windowed(size = malledWidth, step = 1).forEachIndexed { i1, window1 ->
        val shift = i1 + malledWidth
        holes.toList().drop(shift).windowed(size = malledWidth, step = 1).forEachIndexed { i2, window2 ->
            val sum = window1.sum() + window2.sum()
            if (sum > max) {
                max = sum
                maxIndex1 = i1
                maxIndex2 = i2 + shift
            }
        }
    }
    return Triple(maxIndex1, maxIndex2, max)
}
