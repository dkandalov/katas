package eightQueen

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class EightQueen18 {
    @Test fun `generate all possible boards with queens in such positions that do not attack each other`() {
        assertThat(generateBoards(boardSize = 0).toString(), equalTo("[Board(queens=[])]"))
        assertThat(generateBoards(boardSize = 1).toString(), equalTo("[Board(queens=[0,0])]"))
        assertThat(generateBoards(boardSize = 2).toString(), equalTo("[]"))
        assertThat(generateBoards(boardSize = 3).toString(), equalTo("[]"))
        assertThat(generateBoards(boardSize = 4).toString(), equalTo("[Board(queens=[0,1, 1,3, 2,0, 3,2]), Board(queens=[0,2, 1,0, 2,3, 3,1])]"))
    }

    @Test fun `queen position is valid for a board`() {
        assertTrue(Board().isValidMove(Queen(0, 0)))
        assertTrue(Board(Queen(0, 0)).isValidMove(Queen(1, 2)))
        assertFalse(Board(Queen(0, 0)).isValidMove(Queen(0, 1)))
        assertFalse(Board(Queen(0, 0)).isValidMove(Queen(1, 1)))
    }

    private data class Queen(val x: Int, val y: Int) {
        override fun toString() = "$x,$y"
    }

    private data class Board(val queens: List<Queen> = emptyList()) {
        constructor(vararg queens: Queen) : this(queens.asList())

        fun add(queen: Queen) = if (isValidMove(queen)) Board(queens + queen) else null

        fun isValidMove(queen: Queen) =
                queens.none{ it.x == queen.x || it.y == queen.y } &&
                queens.none{ Math.abs(it.x - queen.x) == Math.abs(it.y - queen.y) }
    }

    private fun generateBoards(boardSize: Int): List<Board> {
        var boards = listOf(Board())
        0.until(boardSize).forEach { x ->
            boards = boards.flatMap { board ->
                0.until(boardSize).map { y -> board.add(Queen(x, y)) }
            }.filterNotNull()
        }
        return boards
    }
}