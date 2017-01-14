package katas.kotlin.hanoi

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test

class Hanoi1 {
    @Test fun `find moves to solve Hanoi puzzle`() {
        assertThat(findHanoiSolution(1), equalTo(listOf(
                Move(0, -1)
        )))
        assertThat(findHanoiSolution(2), equalTo(listOf(
                Move(0, 1), Move(1, -1), Move(0, 1)
        )))
        assertThat(findHanoiSolution(3), equalTo(listOf(
                Move(0, -1), Move(1, 1), Move(0, -1),
                Move(2, -1),
                Move(0, -1), Move(1, 1), Move(0, -1)
        )))

        assertThat(findHanoiSolution(1).toPositionMoves(1), equalTo(listOf(
                PosMove(0, -1)
        )))
        assertThat(findHanoiSolution(2).toPositionMoves(2), equalTo(listOf(
                PosMove(0, 1), PosMove(0, -1), PosMove(1, 1)
        )))
        assertThat(findHanoiSolution(3).toPositionMoves(3), equalTo(listOf(
                PosMove(0, -1), PosMove(0, 1), PosMove(2, -1),
                PosMove(0, -1),
                PosMove(1, -1), PosMove(1, 1), PosMove(0, -1)
        )))
    }

    private data class Move(val plate: Int, val direction: Int)

    private fun findHanoiSolution(size: Int, direction: Int = -1): List<Move> {
        if (size == 1) return listOf(Move(0, direction))
        val solution = findHanoiSolution(size - 1, -direction)
        return solution + Move(size - 1, direction) + solution
    }

    private data class PosMove(val position: Int, val direction: Int)

    private fun List<Move>.toPositionMoves(size: Int): List<PosMove> {
        val poles = mutableListOf(
                (size-1).downTo(0).map { it },
                listOf(),
                listOf()
        )
        return this.map { move ->
            val position = poles.indexOfFirst { it.isNotEmpty() && it.last() == move.plate }
            if (position == -1) throw IllegalStateException("Couldn't make move: $move with poles: $poles")
            val newPosition = (position + move.direction + poles.size) % poles.size

            poles[position] = poles[position].dropLast(1)
            poles[newPosition] = poles[newPosition] + move.plate

            PosMove(position, move.direction)
        }
    }
}