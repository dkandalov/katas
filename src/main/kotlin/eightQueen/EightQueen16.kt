package eightQueen

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class EightQueenTest {
    @Test fun `find all possible queen positions on a board from which they do no attack each other`() {
        assertThat(findPositions(boardSize = 0).size, equalTo(1))
        assertThat(findPositions(boardSize = 1).size, equalTo(1))
        assertThat(findPositions(boardSize = 2).size, equalTo(0))
        assertThat(findPositions(boardSize = 3).size, equalTo(0))
        assertThat(findPositions(boardSize = 4), equalTo(listOf(
                Positions(listOf(Queen(2, 3), Queen(0, 2), Queen(3, 1), Queen(1, 0))),
                Positions(listOf(Queen(1, 3), Queen(3, 2), Queen(0, 1), Queen(2, 0)))
        )))
        assertThat(findPositions(boardSize = 5).size, equalTo(10))
        assertThat(findPositions(boardSize = 6).size, equalTo(4))
        assertThat(findPositions(boardSize = 7).size, equalTo(40))
        assertThat(findPositions(boardSize = 8).size , equalTo(92))
        assertThat(findPositions(boardSize = 9).size , equalTo(352))
        assertThat(findPositions(boardSize = 10).size , equalTo(724))
    }
}

private data class Queen(val x: Int, val y: Int) {
    override fun toString() = "($x,$y)"
}

private data class Positions(val queens: List<Queen> = emptyList()) {
    fun add(queen: Queen) = if (isValidMove(queen)) Positions(queens + queen) else null

    private fun isValidMove(queen: Queen) =
            queens.none{ it.x == queen.x || it.y == queen.y } &&
            queens.none{ Math.abs(it.x - queen.x) == Math.abs(it.y - queen.y) }
}

private fun findPositions(boardSize: Int, y: Int = 0): List<Positions> {
    if (y == boardSize) return listOf(Positions())
    val positions = findPositions(boardSize, y + 1)
    return 0.until(boardSize).flatMap { x -> positions.mapNotNull{ it.add(Queen(x, y)) } }
}