package katas.kotlin.eightQueen

import kotlincommon.test.shouldEqual
import org.junit.Test

class EightQueen23 {
    @Test fun `find positions of queens on board in which they don't attack each other`() {
        Board(size = 0).doFindQueenPositions() shouldEqual listOf()
        Board(size = 1).doFindQueenPositions() shouldEqual listOf(Board(size = 1, queens = listOf(Queen(0, 0))))
        Board(size = 2).doFindQueenPositions() shouldEqual listOf()
        Board(size = 3).doFindQueenPositions() shouldEqual listOf()
        Board(size = 4).doFindQueenPositions() shouldEqual listOf(
            Board(size = 4, queens = listOf(Queen(x = 0, y = 1), Queen(x = 1, y = 3), Queen(x = 2, y = 0), Queen(x = 3, y = 2))),
            Board(size = 4, queens = listOf(Queen(x = 0, y = 2), Queen(x = 1, y = 0), Queen(x = 2, y = 3), Queen(x = 3, y = 1)))
        )
        Board(size = 8).doFindQueenPositions().size shouldEqual 92
        Board(size = 10).doFindQueenPositions().size shouldEqual 724

        Board(size = 20).findQueenPositions().take(1).toList() shouldEqual listOf(
            Board(size = 20, queens = listOf(
                Queen(x = 0, y = 0), Queen(x = 1, y = 2), Queen(x = 2, y = 4), Queen(x = 3, y = 1), Queen(x = 4, y = 3),
                Queen(x = 5, y = 12), Queen(x = 6, y = 14), Queen(x = 7, y = 11), Queen(x = 8, y = 17), Queen(x = 9, y = 19),
                Queen(x = 10, y = 16), Queen(x = 11, y = 8), Queen(x = 12, y = 15), Queen(x = 13, y = 18), Queen(x = 14, y = 7),
                Queen(x = 15, y = 9), Queen(x = 16, y = 6), Queen(x = 17, y = 13), Queen(x = 18, y = 5), Queen(x = 19, y = 10)
            ))
        )
    }

    private fun Board.doFindQueenPositions(): List<Board> = findQueenPositions().toList()

    private fun Board.findQueenPositions(): Sequence<Board> {
        val nextMoves = sequence {
            val x = (queens.map { it.x }.max() ?: -1) + 1
            0.until(size)
                .map { y -> Queen(x, y) }
                .filter { isValidMove(it) }
                .forEach { yield(it) }
        }
        return nextMoves.flatMap { move ->
            val newBoard = copy(queens = queens + move)
            if (newBoard.queens.size == size) sequenceOf(newBoard)
            else newBoard.findQueenPositions()
        }
    }

    private fun Board.isValidMove(queen: Queen) =
        queens.size < size &&
            queens.none { it.x == queen.x || it.y == queen.y } &&
            queens.none { Math.abs(it.x - queen.x) == Math.abs(it.y - queen.y) }

    private data class Board(val size: Int, val queens: List<Queen> = emptyList())

    private data class Queen(val x: Int, val y: Int)
}