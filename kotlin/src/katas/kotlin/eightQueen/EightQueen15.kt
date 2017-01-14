package katas.kotlin.eightQueen

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test
import java.util.*

class EightQueen15 {
    @Test fun `find queen positions on board`() {
        (0..8).forEach { boardSize ->
            val solutions = queenPositions(boardSize).toList()
            val iterativeSolutions = queenPositionsIterative(boardSize).toList()

            println(solutions)
            println(iterativeSolutions)
            println(solutions.size)

            solutions.zip(iterativeSolutions.toList()).forEach {
                println(it.first.toPrettyString())
                println("=================")
                assertThat(it.first, equalTo(it.second))
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
        fun queenPositions(solution: Solution): Sequence<Solution> {
            if (solution.complete) return sequenceOf(solution)
            else return solution.nextSteps().flatMap(::queenPositions)
        }
        return queenPositions(Solution(boardSize))
    }

    private fun queenPositionsIterative(boardSize: Int): Sequence<Solution> {
        val queue = LinkedList<Solution>()
        queue.add(Solution(boardSize))

        val iterator = object : Iterator<Solution> {
            override fun hasNext(): Boolean {
                while (queue.isNotEmpty() && !queue.first.complete) {
                    queue.addAll(0, queue.remove().nextSteps().toList())
                }
                return queue.isNotEmpty()
            }
            override fun next() = queue.remove()
        }

        return object : Sequence<Solution> { override fun iterator() = iterator }
    }
}
