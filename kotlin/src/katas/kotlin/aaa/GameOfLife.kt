package katas.kotlin.aaa

import katas.kotlin.shouldEqual
import org.hamcrest.core.IsEqual.equalTo
import org.junit.Assert.assertThat
import org.junit.Test

class Cell()
data class Game(val liveCells: List<Cell> = emptyList()) {
    fun evolve(): Game {
        return Game()
    }
}

class GameOfLife {
    @Test fun `empty game has no life cells`() {
        assertThat(Game().liveCells.isEmpty(), equalTo(true))
    }

    @Test fun `game with one live cell has one live cell`() {
        assertThat(Game(listOf(Cell())).liveCells.count(), equalTo(1))
    }

    @Test fun `empty game evolves into an empty game`() {
        Game().evolve() shouldEqual Game()
    }

    @Test fun `game with one cell becomes empty`() {
        Game(liveCells = listOf(Cell())).evolve() shouldEqual Game()
    }
}