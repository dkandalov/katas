package katas.kotlin.eightQueen

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test
import java.lang.Math.abs

class EightQueen12 {
    @Test fun `find positions of queens so that they don't attack each other`() {
        assertThat(queenPositions(boardSize = 0), equalTo(setOf(emptySet())))
        assertThat(queenPositions(boardSize = 1), equalTo(setOf(setOf(Queen(0, 0)))))
        assertThat(queenPositions(boardSize = 2), equalTo(emptySet()))
        assertThat(queenPositions(boardSize = 3), equalTo(emptySet()))
        assertThat(queenPositions(boardSize = 4), equalTo(setOf(
                setOf(Queen(3, 1), Queen(2, 3), Queen(1, 0), Queen(0, 2)),
                setOf(Queen(3, 2), Queen(2, 0), Queen(1, 3), Queen(0, 1))
        )))

        assertThat(queenPositions(boardSize = 5).size, equalTo(10))
        assertThat(queenPositions(boardSize = 8).size, equalTo(92))
    }

    private data class Queen(val column: Int, val row: Int) {
        override fun toString() = "($column,$row)"
    }

    private fun queenPositions(boardSize: Int, column: Int = 0): Set<Set<Queen>> {
        if (column == boardSize) return setOf(emptySet())
        val rows = 0 until boardSize
        val positions = queenPositions(boardSize, column + 1)
        return positions.flatMap { queens ->
            rows.mapNotNull { row ->
                val newQueen = Queen(column, row)
                if (isValidMove(newQueen, queens)) queens + newQueen else null
            }
        }.toSet()
    }

    private fun isValidMove(queen: Queen, queens: Set<Queen>): Boolean {
        return queens.none { it.column == queen.column || it.row == queen.row } &&
               queens.none { abs(it.column - queen.column) == abs(it.row - queen.row) }
    }
}