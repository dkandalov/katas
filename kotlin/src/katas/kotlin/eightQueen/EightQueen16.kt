package katas.kotlin.eightQueen

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class EightQueen16 {
    @Test fun `find all possible queen positions on a board from which they do no attack each other`() {
        assertThat(findBoards(boardSize = 0).size, equalTo(1))
        assertThat(findBoards(boardSize = 1).size, equalTo(1))
        assertThat(findBoards(boardSize = 2).size, equalTo(0))
        assertThat(findBoards(boardSize = 3).size, equalTo(0))
        assertThat(findBoards(boardSize = 4), equalTo(listOf(
            Board(listOf(Queen(2, 3), Queen(0, 2), Queen(3, 1), Queen(1, 0))),
            Board(listOf(Queen(1, 3), Queen(3, 2), Queen(0, 1), Queen(2, 0)))
        )))
        assertThat(findBoards(boardSize = 5).size, equalTo(10))
        assertThat(findBoards(boardSize = 6).size, equalTo(4))
        assertThat(findBoards(boardSize = 7).size, equalTo(40))
        assertThat(findBoards(boardSize = 8).size, equalTo(92))
        assertThat(findBoards(boardSize = 9).size, equalTo(352))
        assertThat(findBoards(boardSize = 10).size, equalTo(724))
    }

    @Test fun `both recursive and iterative functions should produce the same result`() {
        (0..10).forEach { boardSize ->
            assertThat(findBoards(boardSize), equalTo(findBoards_iterative(boardSize)))
        }
    }
}

private data class Queen(val x: Int, val y: Int) {
    override fun toString() = "($x,$y)"
}

private data class Board(val queens: List<Queen> = emptyList()) {
    fun add(queen: Queen) = if (isValidMove(queen)) Board(queens + queen) else null

    private fun isValidMove(queen: Queen) =
        queens.none { it.x == queen.x || it.y == queen.y } &&
            queens.none { Math.abs(it.x - queen.x) == Math.abs(it.y - queen.y) }
}

private fun findBoards(boardSize: Int, y: Int = 0): List<Board> {
    if (y == boardSize) return listOf(Board())
    val boards = findBoards(boardSize, y + 1)
    return 0.until(boardSize)
        .map { x -> Queen(x, y) }
        .flatMap { queen -> boards.mapNotNull { it.add(queen) } }
}

private fun findBoards_iterative(boardSize: Int): List<Board> {
    var boards = listOf(Board())
    0.until(boardSize).reversed().forEach { y ->
        boards = 0.until(boardSize)
            .map { x -> Queen(x, y) }
            .flatMap { queen -> boards.mapNotNull { it.add(queen) } }
    }
    return boards
}
