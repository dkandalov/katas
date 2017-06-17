package katas.kotlin.eightQueen

import katas.kotlin.shouldEqual
import org.junit.Test
import kotlin.coroutines.experimental.buildSequence

class EightQueen21 {
    @Test fun `find positions of queens so that they don't attack each other`() {
        Board(size = 0).findPositions().toList() shouldEqual listOf(Board(size = 0))
        Board(size = 1).findPositions().toList() shouldEqual listOf(Board(size = 1, queens = listOf(Queen(0, 0))))
        Board(size = 2).findPositions().toList() shouldEqual emptyList()
        Board(size = 3).findPositions().toList() shouldEqual emptyList()
        Board(size = 4).findPositions().toList() shouldEqual listOf(
            Board(size = 4, queens = listOf(Queen(0, 2), Queen(1, 0), Queen(2, 3), Queen(3, 1))),
            Board(size = 4, queens = listOf(Queen(0, 1), Queen(1, 3), Queen(2, 0), Queen(3, 2)))
        )
        Board(size = 8).findPositions().toList().size shouldEqual 92
    }

    @Test fun `generate all possible queen positions`() {
        Board(size = 1).allPossiblePositions().toList() shouldEqual listOf(Queen(0, 0))
        Board(size = 2).allPossiblePositions().toList() shouldEqual listOf(Queen(0, 0), Queen(0, 1), Queen(1, 0), Queen(1, 1))
    }

    @Test fun `determine valid queen positions`() {
        Board(size = 4, queens = listOf(Queen(0, 0))).apply {
            add(Queen(0, 0)) shouldEqual null
            add(Queen(0, 2)) shouldEqual null
            add(Queen(2, 0)) shouldEqual null
            add(Queen(2, 2)) shouldEqual null
            add(Queen(1, 2)) shouldEqual Board(size = 4, queens = listOf(Queen(0, 0), Queen(1, 2)))
        }
    }

    private data class Board(val size: Int, val queens: List<Queen> = emptyList()) {
        fun findPositions(): Sequence<Board> = buildSequence {
            var boards = listOf(Board(size))
            allPossiblePositions().forEach { queen ->
                boards += boards.mapNotNull { it.add(queen) }.toList()
            }
            boards.filter { it.size == it.queens.size }.forEach { yield(it) }
        }

        fun allPossiblePositions(): Sequence<Queen> = buildSequence {
            0.until(size).forEach { x ->
                0.until(size).forEach { y ->
                    yield(Queen(x, y))
                }
            }
        }

        fun add(queen: Queen): Board? = if (isValidMove(queen)) copy(queens = queens + queen) else null

        private fun isValidMove(queen: Queen) =
            queens.size < size &&
            queens.none { it.x == queen.x || it.y == queen.y } &&
            queens.none { Math.abs(it.x - queen.x) == Math.abs(it.y - queen.y) }
    }

    private data class Queen(val x: Int, val y: Int) {
        override fun toString() = "$x,$y"
    }
}