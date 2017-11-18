package katas.kotlin.aaa

import katas.kotlin.shouldEqual
import org.hamcrest.core.IsEqual.equalTo
import org.junit.Assert.assertThat
import org.junit.Test

data class Cell(val x: Int, val y: Int)
data class Game(val liveCells: List<Cell> = emptyList()) {
    fun evolve(): Game {
        return Game(if (liveCells.size == 1) emptyList() else liveCells)
    }

    fun numberOfNeighbours(x: Int, y: Int): Int {
        val shifts = listOf(
            Pair(0, 1), Pair(0, -1),
            Pair(1, 0), Pair(-1, 0)
        )
        return shifts
            .map { Cell(x + it.first, y + it.second) }
            .filter { liveCells.contains(it) }
            .count()
    }
}

class GameOfLife {
    @Test fun `empty game has no life cells`() {
        assertThat(Game().liveCells.isEmpty(), equalTo(true))
    }

    @Test fun `game with one live cell has one live cell`() {
        assertThat(Game(listOf(Cell(0, 0))).liveCells.count(), equalTo(1))
    }

    @Test fun `empty game evolves into an empty game`() {
        Game().evolve() shouldEqual Game()
    }

    @Test fun `game with one cell becomes empty`() {
        Game(liveCells = listOf(Cell(0, 0))).evolve() shouldEqual Game()
    }

    @Test fun `square becomes a square`() {
        val liveCells = listOf(
            Cell(0, 0),
            Cell(0, 1),
            Cell(1, 0),
            Cell(1, 1)
        )
        Game(liveCells).evolve() shouldEqual Game(liveCells)
    }

    @Test fun `amount of live neighbours in empty game is zero`() {
        Game().numberOfNeighbours(0, 0) shouldEqual 0
    }

    @Test fun `amount of neighbours is one in game with one live cell`() {
        Game(listOf(Cell(0, 0))).numberOfNeighbours(0, 1) shouldEqual 1
    }

    @Test fun `count neighbours vertically in game with multiple live cells`() {
        val game = Game(listOf(Cell(0, 0), Cell(0, 1), Cell(0, 2)))
        game.numberOfNeighbours(0, 0) shouldEqual 1
        game.numberOfNeighbours(0, 1) shouldEqual 2
        game.numberOfNeighbours(0, 2) shouldEqual 1
    }

    @Test fun `count neighbours horizontally in game with multiple live cells`() {
        val game = Game(listOf(Cell(0, 0), Cell(1, 0), Cell(2, 0)))
        game.numberOfNeighbours(0, 0) shouldEqual 1
        game.numberOfNeighbours(1, 0) shouldEqual 2
        game.numberOfNeighbours(2, 0) shouldEqual 1
        game.numberOfNeighbours(40, 0) shouldEqual 0
    }

    @Test fun `count all neighbours in game with multiple live cells`() {
        val game = Game(listOf(Cell(1, 1)))
        game.numberOfNeighbours(1, 0) shouldEqual 1
        game.numberOfNeighbours(0, 1) shouldEqual 1
        game.numberOfNeighbours(1, 2) shouldEqual 1
        game.numberOfNeighbours(2, 1) shouldEqual 1

        game.numberOfNeighbours(1, 1) shouldEqual 0
        game.numberOfNeighbours(40, 0) shouldEqual 0
    }
}