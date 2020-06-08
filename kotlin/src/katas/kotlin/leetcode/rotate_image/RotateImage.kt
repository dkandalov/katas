package katas.kotlin.leetcode.rotate_image

import datsok.shouldEqual
import org.junit.Test

class RotateImageTests {
    @Test fun `rotate clockwise 90 degrees`() {
        mutableListOf(
            mutableListOf(1, 2, 3),
            mutableListOf(4, 5, 6),
            mutableListOf(7, 8, 9)
        ).rotateClockwise() shouldEqual mutableListOf(
            mutableListOf(7, 4, 1),
            mutableListOf(8, 5, 2),
            mutableListOf(9, 6, 3)
        )

        mutableListOf(
            mutableListOf(5, 1, 9, 11),
            mutableListOf(2, 4, 8, 10),
            mutableListOf(13, 3, 6, 7),
            mutableListOf(15, 14, 12, 16)
        ).rotateClockwise() shouldEqual mutableListOf(
            mutableListOf(15, 13, 2, 5),
            mutableListOf(14, 3, 4, 1),
            mutableListOf(12, 6, 8, 9),
            mutableListOf(16, 7, 10, 11)
        )
    }
}

private typealias Image = MutableList<MutableList<Int>>

private val Image.width: Int get() = size
private val Image.height: Int get() = first().size

private fun Image.flipDiagonally(): Image {
    0.until(height).forEach { row ->
        row.until(width).forEach { column ->
            val tmp = this[row][column]
            this[row][column] = this[column][row]
            this[column][row] = tmp
        }
    }
    return this
}

private fun Image.flipVertically(): Image {
    0.until(height).forEach { row ->
        0.until(width / 2).forEach { column ->
            val tmp = this[row][column]
            this[row][column] = this[row][width - 1 - column]
            this[row][width - 1 - column] = tmp
        }
    }
    return this
}

private fun Image.rotateClockwise(): Image {
    flipDiagonally()
    flipVertically()
    return this
}
