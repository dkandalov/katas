package eightQueen

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test
import java.lang.Math.abs

class EightQueen {
    @Test fun `find all possible positions of queens on chessboard so that they don't attack each other`() {
        assertThat(findAllPositions(0).size, equalTo(1))
        assertThat(findAllPositions(1).size, equalTo(1))
        assertThat(findAllPositions(3).size, equalTo(0))
        assertThat(findAllPositions(4).map{ it.toStringBoard(4) }, equalTo(listOf(
                "-Q--\n" +
                "---Q\n" +
                "Q---\n" +
                "--Q-",

                "--Q-\n" +
                "Q---\n" +
                "---Q\n" +
                "-Q--"
        )))
        assertThat(findAllPositions(5).size, equalTo(10))
        assertThat(findAllPositions(8).size, equalTo(92))

        0.rangeTo(8).forEach { boardSize ->
            findAllPositions(boardSize).forEach {
                println(it.toStringBoard(boardSize))
                println()
            }
        }
    }

    private data class Queen(val column: Int, val row: Int)

    private data class Positions(val queens: List<Queen>) {
        fun isValidToAdd(queen: Queen): Boolean {
            return queens.none{ it.column == queen.column || it.row == queen.row } &&
                   queens.none{ abs(it.column - queen.column) == abs(it.row - queen.row) }
        }

        fun add(queen: Queen) = Positions(queens + queen)

        fun toStringBoard(boardSize: Int): String {
            return 0.until(boardSize).map { row ->
                0.until(boardSize).map{ column ->
                    if (queens.contains(Queen(column, row))) "Q" else "-"
                }.joinToString("")
            }.joinToString("\n")
        }
    }

    private fun findAllPositions(boardSize: Int, column: Int = 0): List<Positions> {
        if (column == boardSize) return listOf(Positions(emptyList()))
        return findAllPositions(boardSize, column + 1).flatMap { positions ->
            0.until(boardSize).mapNotNull { row ->
                val newQueen = Queen(column, row)
                if (positions.isValidToAdd(newQueen)) positions.add(newQueen) else null
            }
        }
    }
}