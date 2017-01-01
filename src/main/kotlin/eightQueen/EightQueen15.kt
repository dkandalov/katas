package eightQueen

import org.junit.Test

class EightQueen15 {
    @Test fun `find queen positions on board`() {
        (0..8).forEach { boardSize ->
            val solutions = queenPositions(boardSize).toList()
            println(solutions)
            println(solutions.size)
            solutions.forEach {
                println(it.toPrettyString())
                println("=================")
            }
        }
    }

    private fun EightQueen15.Solution.toPrettyString(): String {
        return 0.until(boardSize).map { row ->
            0.until(boardSize).map { column ->
                if (queens.contains(Queen(column, row))) "Q" else "-"
            }.joinToString("")
        }.joinToString("\n")
    }

    private data class Queen(val column: Int, val row: Int) {
        override fun toString() = "($column,$row)"
    }
    
    private data class Solution(val boardSize: Int, val queens: List<Queen> = emptyList()) {
        val complete: Boolean
            get() = boardSize == queens.size

        fun nextSteps(): Sequence<Solution> {
            val column = (queens.map{ it.column }.max() ?: -1) + 1
            return 0.until(boardSize).asSequence()
                    .map{ row -> Queen(column, row) }
                    .filter{ isValidStep(it) }
                    .map{ Solution(boardSize, queens + it) }
        }

        private fun isValidStep(queen: Queen): Boolean {
            return queens.none{ it.row == queen.row || it.column == queen.column} &&
                   queens.none { Math.abs(it.row - queen.row) == Math.abs(it.column - queen.column) }
        }
    }

    private fun queenPositions(boardSize: Int): Sequence<Solution> {
        return queenPositions(Solution(boardSize))
    }

    private fun queenPositions(solution: Solution): Sequence<Solution> {
        if (solution.complete) return sequenceOf(solution)
        return solution.nextSteps().flatMap{ queenPositions(it) }
    }
}
