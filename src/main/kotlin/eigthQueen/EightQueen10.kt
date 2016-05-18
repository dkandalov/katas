package eigthQueen

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.lang.Math.abs

class EightQueen10 {
    @Test fun `find all solutions for eight queen puzzle`() {
        assertThat(findAllSolutions(boardSize = 0), equalTo(listOf(
                Board()
        )))
        assertThat(findAllSolutions(boardSize = 1), equalTo(listOf(
                Board(Queen(0, 0))
        )))
        assertThat(findAllSolutions(boardSize = 2), equalTo(listOf()))
        assertThat(findAllSolutions(boardSize = 3), equalTo(listOf()))
        assertThat(findAllSolutions(boardSize = 4), equalTo(listOf(
                Board(Queen(1, 0), Queen(3, 1), Queen(0, 2), Queen(2, 3)),
                Board(Queen(2, 0), Queen(0, 1), Queen(3, 2), Queen(1, 3))
        )))
        assertThat(findAllSolutions(boardSize = 8).size, equalTo(92))
    }

    private fun findAllSolutions(boardSize: Int): List<Board> {
        return findAllSolutions(Board(boardSize, listOf()))
    }

    private fun findAllSolutions(board: Board): List<Board> {
        if (board.isComplete()) return listOf(board)
        return board.nextMoves().flatMap { nextSolution ->
            findAllSolutions(nextSolution)
        }
    }

    private data class Queen(val row: Int, val column: Int)

    private data class Board(val size: Int, val queens: List<Queen>) {
        constructor(vararg queens: Queen): this(maxPosition(queens), queens.toList()) {}

        fun nextMoves(): List<Board> {
            val nextColumn = (queens.map{ it.column }.max() ?: -1) + 1
            return (0..size-1)
                .map{ Queen(it, nextColumn) }
                .filter{ isValidMove(it) }
                .map{ Board(size, queens + it) }
        }

        fun isComplete(): Boolean {
            return queens.size == size
        }

        private fun isValidMove(queen: Queen): Boolean {
            val notOnTheSameLine = queens.none { it.row == queen.row || it.column == queen.column }
            val notOnTheSameDiagonal = queens.none {
                abs(it.row - queen.row) == abs(it.column - queen.column)
            }
            return notOnTheSameLine && notOnTheSameDiagonal
        }

        companion object {
            private fun maxPosition(queens: Array<out Queen>): Int {
                if (queens.isEmpty()) return 0
                return Math.max(queens.map{ it.row }.max()!!, queens.map{ it.column }.max()!!) + 1
            }
        }
    }
}
