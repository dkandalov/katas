package katas.kotlin.aaa

import katas.kotlin.shouldEqual
import org.hamcrest.core.IsEqual.equalTo
import org.junit.Assert.assertThat
import org.junit.Test

data class Cell(val isAlive: Boolean)
data class Game(val cells: List<Cell> = emptyList()) {
    fun evolve(): Game {
        return this
    }
}

class GameOfLife {
    @Test fun `empty game has no life cells`() {
        assertThat(Game().cells.none { it.isAlive }, equalTo(true))
    }

    @Test fun `game with one live cell has one live cell`() {
        assertThat(Game(listOf(Cell(true))).cells.count { it.isAlive }, equalTo(1))
    }

    @Test fun `empty game evolves into an empty game`() {
        Game().evolve() shouldEqual Game()
    }
}