package katas.kotlin.gameoflife

import katas.kotlin.shouldEqual
import org.junit.Ignore
import org.junit.Test

data class Cell(val x: Int, val y: Int) {
    override fun toString() = "$x,$y"
}

data class Universe(val livingCells: List<Cell> = emptyList()) {
    fun evolve(): Universe {
        val oldLivingCells = livingCells
            .mapNotNull { cell ->
                val n = neighboursOf(cell.x, cell.y)
                if (n == 2 || n == 3) cell else null
            }
        // val cellsThatCameAlive =
        return Universe(oldLivingCells)
    }

    fun neighboursOf(x: Int, y: Int): Int {
        val right = Pair(1, 0)
        val bottomRight = Pair(1, 1)
        val bottom = Pair(0, 1)
        val bottomLeft = Pair (-1, 1)
        val left = Pair(-1, 0)
        val topLeft = Pair(-1, -1)
        val top = Pair(0, -1)
        val topRight = Pair(1, -1)
        val shifts = listOf(right, bottomRight, bottom, bottomLeft, left, topLeft, top, topRight)

        return shifts
            .map { shift -> Cell(x + shift.first, y + shift.second) }
            .filter{ livingCells.contains(it) }
            .size
    }
}

class GameOfLife {
    @Test fun `empty universe evolves into empty universe`() {
        Universe().evolve() shouldEqual Universe()
    }

    @Test fun `live cell with fewer than two live neighbours dies, as if caused by underpopulation`() {
        Universe(livingCells = listOf(Cell(0, 0))).evolve() shouldEqual Universe()
    }

    @Test fun `live cell with two or three live neighbours lives on to the next generation`() {
        // xx-  ==>  xx-
        // x--       x--
        Universe(livingCells = listOf(
            Cell(0, 0),
            Cell(0, 1),
            Cell(1, 0)
        )).evolve() shouldEqual Universe(listOf(
            Cell(0, 0),
            Cell(0, 1),
            Cell(1, 0)
        ))
    }

    @Test fun `live cell with more than three live neighbours dies, as if by overpopulation`() {
        // xxx  ==>  x-x
        // xxx       ---
        // xxx       x-x
        Universe(livingCells = listOf(
            Cell(0, 0),
            Cell(0, 1),
            Cell(0, 2),
            Cell(1, 0),
            Cell(1, 1),
            Cell(1, 2),
            Cell(2, 0),
            Cell(2, 1),
            Cell(2, 2)
        )).evolve() shouldEqual Universe(listOf(
            Cell(0, 0),
            Cell(0, 2),
            Cell(2, 0),
            Cell(2, 2)
        ))
    }

    @Ignore // unfinished
    @Test fun `dead cell with exactly three live neighbours becomes a live cell, as if by reproduction`() {
        val universe = """
        |xx
        |x-
        """.parseAsUniverse()

        universe.evolve() shouldEqual """
        |xx
        |xx
        """.parseAsUniverse()
    }

    @Test fun `count amount of cell neighbours`() {
        // xx-
        // x--
        val universe = Universe(livingCells = listOf(
            Cell(0, 0),
            Cell(0, 1),
            Cell(1, 0)
        ))
        universe.neighboursOf(0, 0) shouldEqual 2
        universe.neighboursOf(1, 0) shouldEqual 2
        universe.neighboursOf(0, 1) shouldEqual 2
        universe.neighboursOf(0, 2) shouldEqual 1
        universe.neighboursOf(1, 1) shouldEqual 3
    }

    @Test fun `can parse string as Universe`() {
        val universe = """
        |-xx
        |x-x
        |xx-
        """.parseAsUniverse()

        universe shouldEqual Universe(listOf(
            Cell(1, 0),
            Cell(2, 0),
            Cell(0, 1),
            Cell(2, 1),
            Cell(0, 2),
            Cell(1, 2)
        ))
    }
}

private fun String.parseAsUniverse(): Universe {
    val cells = trimMargin("|").split("\n").mapIndexed { y, line ->
        line.toCharArray().mapIndexed { x, char ->
            if (char == 'x') Cell(x, y) else null
        }.filterNotNull()
    }.flatten()
    return Universe(cells)
}
