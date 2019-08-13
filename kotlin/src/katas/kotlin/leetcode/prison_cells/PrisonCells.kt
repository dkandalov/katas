package katas.kotlin.leetcode.prison_cells

import kotlincommon.test.shouldEqual
import org.junit.Test

class PrisonCellsTests {
    @Test fun examples() {
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 0) shouldEqual intArrayOf(0, 1, 0, 1, 1, 0, 0, 1)
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 1) shouldEqual intArrayOf(0, 1, 1, 0, 0, 0, 0, 0)
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 2) shouldEqual intArrayOf(0, 0, 0, 0, 1, 1, 1, 0)
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 3) shouldEqual intArrayOf(0, 1, 1, 0, 0, 1, 0, 0)
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 4) shouldEqual intArrayOf(0, 0, 0, 0, 0, 1, 0, 0)
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 5) shouldEqual intArrayOf(0, 1, 1, 1, 0, 1, 0, 0)
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 6) shouldEqual intArrayOf(0, 0, 1, 0, 1, 1, 0, 0)
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 7) shouldEqual intArrayOf(0, 0, 1, 1, 0, 0, 0, 0)
    }
}

private tailrec fun prisonAfterNDays(cells: IntArray, days: Int): IntArray {
    if (days == 0) return cells

    val newCells = IntArray(8) { i ->
        when {
            i == 0 || i == 7             -> 0
            cells[i - 1] == cells[i + 1] -> 1
            else                         -> 0
        }
    }
    return prisonAfterNDays(newCells, days - 1)
}