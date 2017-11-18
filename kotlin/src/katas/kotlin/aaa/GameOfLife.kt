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

    fun numberOfNeighbours(i: Int, i1: Int): Any {
        return 0
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
    @Test fun `amount of neighbours is zero`() {
        Game().numberOfNeighbours(0,0) shouldEqual 0
    }
}