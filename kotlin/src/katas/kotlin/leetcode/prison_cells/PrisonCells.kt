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

    @Test fun `cells after many days`() {
        prisonAfterNDays(intArrayOf(0, 1, 0, 1, 1, 0, 0, 1), days = 1_000_000_000)
    }
}

private fun prisonAfterNDays(cells: IntArray, days: Int): IntArray {
    return prisonAfterNDays(cells.toInt(), days).toIntArray()
}

private fun prisonAfterNDays(cells: Int, days: Int): Int {
    if (days == 0) return cells

    var dayCount = days
    var oldCells = cells
    var newCells = 0
    while (dayCount-- > 0) {
        newCells = 0
        if (oldCells.and(128) == oldCells.and(32).shl(2)) newCells = newCells.or(64)
        if (oldCells.and(64) == oldCells.and(16).shl(2)) newCells = newCells.or(32)
        if (oldCells.and(32) == oldCells.and(8).shl(2)) newCells = newCells.or(16)
        if (oldCells.and(16) == oldCells.and(4).shl(2)) newCells = newCells.or(8)
        if (oldCells.and(8) == oldCells.and(2).shl(2)) newCells = newCells.or(4)
        if (oldCells.and(4) == oldCells.and(1).shl(2)) newCells = newCells.or(2)
        oldCells = newCells
    }
    return newCells
}

private tailrec fun prisonAfterNDays_(cells: IntArray, days: Int): IntArray {
    if (days == 0) return cells

    val newCells = IntArray(8) { i ->
        when {
            i == 0 || i == 7             -> 0
            cells[i - 1] == cells[i + 1] -> 1
            else                         -> 0
        }
    }
    return prisonAfterNDays_(newCells, days - 1)
}

private fun IntArray.toInt(): Int {
    return 0
        .or(this[0]).shl(1)
        .or(this[1]).shl(1)
        .or(this[2]).shl(1)
        .or(this[3]).shl(1)
        .or(this[4]).shl(1)
        .or(this[5]).shl(1)
        .or(this[6]).shl(1)
        .or(this[7])
}

private fun Int.toIntArray(): IntArray {
    return intArrayOf(
        and(128).shr(7),
        and(64).shr(6),
        and(32).shr(5),
        and(16).shr(4),
        and(8).shr(3),
        and(4).shr(2),
        and(2).shr(1),
        and(1)
    )
}

class IntToIntArrayConversions {
    @Test fun `IntArray to Int`() {
        intArrayOf(0, 0, 0, 0, 0, 0, 0, 1).toInt() shouldEqual 1
        intArrayOf(0, 0, 0, 0, 0, 0, 1, 0).toInt() shouldEqual 2
        intArrayOf(0, 0, 0, 0, 0, 0, 1, 1).toInt() shouldEqual 3
        intArrayOf(0, 0, 0, 0, 0, 1, 0, 0).toInt() shouldEqual 4
        intArrayOf(0, 0, 0, 0, 1, 0, 0, 0).toInt() shouldEqual 8
        intArrayOf(1, 0, 0, 0, 0, 0, 0, 1).toInt() shouldEqual 129
    }

    @Test fun `Int to IntArray`() {
        1.toIntArray() shouldEqual intArrayOf(0, 0, 0, 0, 0, 0, 0, 1)
        2.toIntArray() shouldEqual intArrayOf(0, 0, 0, 0, 0, 0, 1, 0)
        3.toIntArray() shouldEqual intArrayOf(0, 0, 0, 0, 0, 0, 1, 1)
        4.toIntArray() shouldEqual intArrayOf(0, 0, 0, 0, 0, 1, 0, 0)
        8.toIntArray() shouldEqual intArrayOf(0, 0, 0, 0, 1, 0, 0, 0)
        129.toIntArray() shouldEqual intArrayOf(1, 0, 0, 0, 0, 0, 0, 1)
    }
}
