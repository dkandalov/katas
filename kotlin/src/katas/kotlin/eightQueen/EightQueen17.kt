package katas.kotlin.eightQueen

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class EightQueen17 {
    @Test fun `generate all possible queen positions on a board so that they don't attack each other`() {
        assertThat(Board(size = 0).queenPositions(), equalTo(listOf(Board(0))))
        assertThat(Board(size = 1).queenPositions(), equalTo(listOf(Board(1, listOf(Queen(0, 0))))))
        assertThat(Board(size = 2).queenPositions(), equalTo(emptyList()))
        assertThat(Board(size = 3).queenPositions(), equalTo(emptyList()))
        assertThat(Board(size = 4).queenPositions().map(Board::toPrettyString), equalTo(listOf(
            "-Q--\n" +
            "---Q\n" +
            "Q---\n" +
            "--Q-",

            "--Q-\n" +
            "Q---\n" +
            "---Q\n" +
            "-Q--"
        )))
    }

    private data class Queen(val x: Int, val y: Int) {
        override fun toString() = "($x,$y)"
    }

    private data class Board(val size: Int, val queens: List<Queen> = emptyList()) {
        fun queenPositions(): List<Board> {
            var result = listOf(Board(size))
            0.until(size).forEach { x ->
                result = 0.until(size)
                    .map { y -> Queen(x, y) }
                    .flatMap { queen -> result.map { it.add(queen) } }
                    .filterNotNull()
            }
            return result
        }

        private fun add(queen: Queen) = if (isValid(queen)) copy(queens = queens + queen) else null

        private fun isValid(queen: Queen) =
            queens.none { it.x == queen.x || it.y == queen.y } &&
            queens.none { Math.abs(it.x - queen.x) == Math.abs(it.y - queen.y) }

        fun toPrettyString(): String {
            return 0.until(size).map { y ->
                0.until(size).map { x ->
                    if (queens.contains(Queen(x, y))) "Q" else "-"
                }.joinToString("")
            }.joinToString("\n")
        }
    }
}
