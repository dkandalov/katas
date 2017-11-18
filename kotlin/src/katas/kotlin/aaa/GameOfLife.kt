package katas.kotlin.aaa

import org.hamcrest.core.IsEqual.equalTo
import org.junit.Assert.assertThat
import org.junit.Test

data class Cell(val isAlive: Boolean)
data class Game(val cells: List<Cell> = emptyList())

class GameOfLife {
    @Test fun `empty game has no life cells`() {
        assertThat(Game().cells.none { it.isAlive }, equalTo(true))
    }

    @Test fun `game with one live cell has one live cell`() {
        assertThat(Game(listOf(Cell(true))).cells.count { it.isAlive }, equalTo(1))
    }

}