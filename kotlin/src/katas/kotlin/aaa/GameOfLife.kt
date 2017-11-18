package katas.kotlin.aaa

import org.junit.Test

data class Cell(val isAlive: Boolean)
data class Game(val cells: List<Cell> = emptyList())

class GameOfLife {
    @Test fun `empty game has no life cells`() {
        Game().cells.none { it.isAlive }
    }
}