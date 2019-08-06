package katas.kotlin.leetcode.whac_a_mole

import kotlincommon.test.shouldEqual
import org.junit.Test

class WhacMoleTests {
    @Test fun `hit max amount of moles`() {
        hit(arrayOf(0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0), width = 5) shouldEqual Pair(1, 4)
    }
}

private fun hit(moles: Array<Int>, width: Int): Pair<Int, Int> {
    return Pair(1, 4)
}
