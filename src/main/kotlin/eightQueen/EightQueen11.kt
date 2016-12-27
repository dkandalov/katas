package eightQueen

import org.junit.Test
import java.lang.Math.abs

class EightQueen11 {
    @Test fun `find positions for eight queens`() {
        solve(boardSize = 4).forEach {
            println(it)
        }
    }

    private fun solve(boardSize: Int, column: Int = 0): List<List<Pair<Int, Int>>> {
        if (column == boardSize) return listOf(emptyList())
        return solve(boardSize, column + 1).flatMap { solution ->
                0.until(boardSize)
                    .map{ row -> Pair(column, row) }
                    .filter{ isValid(it, solution) }
                    .map{ solution + it }
            }
    }

    private fun isValid(queen: Pair<Int, Int>, solution: List<Pair<Int, Int>>): Boolean {
        if (solution.any{ it.first == queen.first || it.second == queen.second}) return false
        return solution.none { queen2 ->
            abs(queen.first - queen2.first) == abs(queen.second - queen2.second)
        }
    }
}