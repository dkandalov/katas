package katas.kotlin.gameoflife.foo

import katas.kotlin.shouldEqual
import org.junit.Test
import kotlin.math.absoluteValue

class GameOfLifeTests {
    @Test fun `lonely cell dies`() {
        Game(listOf(Cell(1, 1))).evolve() shouldEqual Game(emptyList())
    }

    @Test fun `cell with two neighbours lives`() {
        val game = Game(Cell(1, 1), Cell(1, 2), Cell(2, 1))
        game.evolve().cells.contains(Cell(1, 1)) shouldEqual true
    }

    @Test fun `cell with three neighbours lives`() {
        val game = Game(Cell(1, 1), Cell(1, 2), Cell(2, 1), Cell(2, 2))
        game.evolve().cells.contains(Cell(1, 1)) shouldEqual true
    }

    @Test fun `cell with four neighbours dies`() {
        val game = Game(Cell(1, 1), Cell(1, 2), Cell(2, 1), Cell(2, 2), Cell(1, 0))
        game.evolve().cells.contains(Cell(1, 1)) shouldEqual false
    }

}

data class Game(val cells: List<Cell>) {
    constructor(vararg cells: Cell) : this(cells.toList())

    fun evolve(): Game {
        val newCells = cells.filter { cell ->
            val neighboursCount = neighboursCount(cell, cells)
            neighboursCount == 2 || neighboursCount == 3
        }
        return Game(newCells)
    }

    private fun neighboursCount(cell: Cell, cells: List<Cell>): Int {
        return cells.count {
            it != cell && it.isNeighbourOf(cell)
        }
    }
}

data class Cell(val x: Int, val y: Int) {
    fun isNeighbourOf(cell: Cell) =
        (x - cell.x).absoluteValue < 2 &&
            (y - cell.y).absoluteValue < 2

    override fun toString() = "($x, $y)"
}