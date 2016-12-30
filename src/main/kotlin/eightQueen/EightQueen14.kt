package eightQueen

import org.junit.Test
import java.lang.Math.abs

class EightQueen14 {
    @Test fun `find all queen positions in which they don't attack each other`() {
        0.rangeTo(5).forEach {
            println(findAll(boardSize = it))
        }
    }

    private data class Queen(val column: Int, val row: Int) {
        override fun toString() = "($column,$row)"
    }

    private fun findAll(boardSize: Int, column: Int = 0, result: List<Queen> = emptyList()): List<List<Queen>> {
        if (column == boardSize) return listOf(result)
        return 0.until(boardSize)
            .map{ row -> Queen(row, column) }
            .filter{ queen -> isValid(queen, result) }
            .flatMap { queen ->
                findAll(boardSize, column + 1, result + queen)
            }
    }

    private fun isValid(newQueen: Queen, queens: List<Queen>): Boolean {
        return queens.none{ it.column == newQueen.column || it.row == newQueen.row } &&
               queens.none{ abs(it.column - newQueen.column) == abs(it.row - newQueen.row) }
    }
}