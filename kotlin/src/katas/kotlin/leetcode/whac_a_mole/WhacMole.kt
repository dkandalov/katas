package katas.kotlin.leetcode.whac_a_mole

import kotlincommon.test.shouldEqual
import org.junit.Test

class WhacMoleTests {
    @Test fun `hit max amount of moles`() {
        hit(arrayOf(0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0), width = 4) shouldEqual Pair(1, 3)
        hit(arrayOf(0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0), width = 5) shouldEqual Pair(1, 4)
        hit(arrayOf(0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0), width = 6) shouldEqual Pair(0, 4)
    }
}

private fun hit(moles: Array<Int>, width: Int): Pair<Int, Int> {
    var max = -1
    var maxIndex = -1
    moles.toList().windowed(size = width, step = 1).forEachIndexed { i, window ->
        val sum = window.sum()
        if (sum > max) {
            max = sum
            maxIndex = i
        }
    }
    return Pair(maxIndex, max)
}
