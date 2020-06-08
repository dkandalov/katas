package katas.kotlin.eightQueen

import datsok.shouldEqual
import org.junit.Test

class EightQueen20 {
    @Test fun `generate all boards with queens which do not attack each other`() {
        generateBoards(0).toString() shouldEqual "[Board(queens=[])]"
        generateBoards(1).toString() shouldEqual "[Board(queens=[0,0])]"
        generateBoards(2).toString() shouldEqual "[]"
        generateBoards(3).toString() shouldEqual "[]"
        generateBoards(4).toString() shouldEqual "[Board(queens=[0,1, 1,3, 2,0, 3,2]), Board(queens=[0,2, 1,0, 2,3, 3,1])]"
        generateBoards(5).toString() shouldEqual "[" +
            "Board(queens=[0,0, 1,2, 2,4, 3,1, 4,3]), " +
            "Board(queens=[0,0, 1,3, 2,1, 3,4, 4,2]), " +
            "Board(queens=[0,1, 1,3, 2,0, 3,2, 4,4]), " +
            "Board(queens=[0,1, 1,4, 2,2, 3,0, 4,3]), " +
            "Board(queens=[0,2, 1,0, 2,3, 3,1, 4,4]), " +
            "Board(queens=[0,2, 1,4, 2,1, 3,3, 4,0]), " +
            "Board(queens=[0,3, 1,0, 2,2, 3,4, 4,1]), " +
            "Board(queens=[0,3, 1,1, 2,4, 3,2, 4,0]), " +
            "Board(queens=[0,4, 1,1, 2,3, 3,0, 4,2]), " +
            "Board(queens=[0,4, 1,2, 2,0, 3,3, 4,1])]"
    }

    private fun generateBoards(boardSize: Int): List<Board> {
        var result = listOf(Board(emptyList()))
        0.until(boardSize).forEach { x ->
            result = result.flatMap { board ->
                0.until(boardSize).map { y ->
                    board.add(Queen(x, y))
                }
            }.filterNotNull()
        }
        return result
    }

    private data class Queen(val x: Int, val y: Int) {
        override fun toString() = "$x,$y"
    }

    private data class Board(val queens: List<Queen>) {
        fun add(queen: Queen): Board? = if (isValidMove(queen)) Board(queens + queen) else null

        private fun isValidMove(queen: Queen): Boolean {
            return queens.none{ it.x == queen.x || it.y == queen.y } &&
                queens.none { Math.abs(it.x - queen.x) == Math.abs(it.y - queen.y) }
        }
    }
}