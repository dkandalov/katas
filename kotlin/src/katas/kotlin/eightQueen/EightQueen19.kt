package katas.kotlin.eightQueen

import org.junit.Test
import java.lang.Math.abs

class EightQueenTest19 {
    @Test fun `find all possible positions of queens on a board in which they don't attack each other`() {
        (0..8).forEach {
            println(findPositions(boardSize = it))
        }
    }

    private data class Queen(val x: Int, val y: Int) {
        override fun toString() = "$x,$y"
    }
    
    private fun findPositions(boardSize: Int): List<List<Queen>> {
        var result = listOf(emptyList<Queen>())
        0.until(boardSize).forEach { x ->
            result = result.flatMap { queens ->
                0.until(boardSize)
                    .map{ y -> Queen(x, y) }
                    .filter{ isValidMove(it, queens) }
                    .map{ queens + it }
            }
        }
        return result
    }

    private fun isValidMove(queen: Queen, queens: List<Queen>): Boolean {
        return queens.none{ queen.x == it.x || queen.y == it.y } &&
               queens.none{ abs(queen.x - it.x) == abs(queen.y - it.y) }
    }
}
