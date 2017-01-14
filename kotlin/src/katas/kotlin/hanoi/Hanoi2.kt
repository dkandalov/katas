package katas.kotlin.hanoi

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import katas.kotlin.hanoi.Hanoi2.Direction.Left
import katas.kotlin.hanoi.Hanoi2.Direction.Right
import org.junit.Test

class Hanoi2 {
    @Test fun `find plate moves to solve Tower of Hanoi`() {
        assertThat(findPlateMoves(numberOfPlates = 1), equalTo(HanoiSolution(
                Move(Plate(0), Left)
        )))
        assertThat(findPlateMoves(numberOfPlates = 2), equalTo(HanoiSolution(
                Move(Plate(0), Right),
                Move(Plate(1), Left),
                Move(Plate(0), Right)
        )))
        assertThat(findPlateMoves(numberOfPlates = 3), equalTo(HanoiSolution(
                Move(Plate(0), Left), Move(Plate(1), Right), Move(Plate(0), Left),
                Move(Plate(2), Left),
                Move(Plate(0), Left), Move(Plate(1), Right), Move(Plate(0), Left)
        )))
    }

    private fun findPlateMoves(numberOfPlates: Int, direction: Direction = Left): HanoiSolution {
        if (numberOfPlates == 0) return HanoiSolution()
        if (numberOfPlates == 1) return HanoiSolution(Move(Plate(0), direction))

        val moves = findPlateMoves(numberOfPlates - 1, direction.opposite())
        return moves + Move(Plate(numberOfPlates - 1), direction) + moves
    }

    private data class HanoiSolution(val moves: List<Move>) {
        constructor(vararg moves: Move) : this(moves.toList())

        infix operator fun plus(move: Move) = HanoiSolution(moves + move)

        infix operator fun plus(solution: HanoiSolution) = HanoiSolution(moves + solution.moves)
    }

    private data class Plate(val id: Int)

    private data class Move(val plate: Plate, val direction: Direction)

    enum class Direction {
        Left, Right;

        fun opposite() = if (this == Left) Right else Left
    }
}

