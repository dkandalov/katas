package katas.kotlin.eightQueen

import kotlincommon.test.shouldEqual
import org.junit.Test

class EightQueen22 {

    @Test fun `find all queen positions in which they don't attack each other`() {
        Board(size = 0).findPositions().toList() shouldEqual listOf(Board(size = 0, queens = emptyList()))
        Board(size = 1).findPositions().toList() shouldEqual listOf(Board(size = 1, queens = listOf(Queen(x = 0, y = 0))))
        Board(size = 2).findPositions().toList() shouldEqual emptyList()
        Board(size = 3).findPositions().toList() shouldEqual emptyList()
        Board(size = 4).findPositions().toList() shouldEqual listOf(
            Board(size = 4, queens = listOf(Queen(x = 0, y = 2), Queen(x = 1, y = 0), Queen(x = 2, y = 3), Queen(x = 3, y = 1))),
            Board(size = 4, queens = listOf(Queen(x = 0, y = 1), Queen(x = 1, y = 3), Queen(x = 2, y = 0), Queen(x = 3, y = 2)))
        )
        Board(size = 8).findPositions().toList().size shouldEqual 92
    }

    private data class Board(val size: Int, val queens: List<Queen> = emptyList()) {
        fun findPositions(): Sequence<Board> = sequence {
            var boards = listOf(Board(size))
            0.until(size).forEach { x ->
                0.until(size).forEach { y ->
                    boards += boards.mapNotNull { it.add(Queen(x, y)) }
                }
            }
            boards
                .filter { it.size == it.queens.size }
                .forEach { yield(it) }
        }

        private fun add(queen: Queen) = if (isValidMove(queen)) copy(queens = queens + queen) else null

        private fun isValidMove(queen: Queen) =
            queens.size < size &&
                queens.none { it.x == queen.x || it.y == queen.y } &&
                queens.none { Math.abs(it.x - queen.x) == Math.abs(it.y - queen.y) }
    }

    private data class Queen(val x: Int, val y: Int)
}