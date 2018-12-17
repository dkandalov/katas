package katas.kotlin.gameoflife

import kotlincommon.test.shouldEqual
import org.junit.Test


class Game1 {
    @Test fun `empty universe`() {
        """
            |-
            """.asUniverse().step() shouldEqual
            """
            |-
            """.asUniverse()
    }

    @Test fun `live cell with fewer than two live neighbours dies, as if caused by underpopulation`() {
        """
            |x
            """.asUniverse().step() shouldEqual
            """
            |-
            """.asUniverse()

        """
            |xx
            """.asUniverse().step() shouldEqual
            """
            |--
            """.asUniverse()
    }

    @Test fun `live cell with two or three live neighbours lives on to the next generation`() {
        """
            |---
            |xxx
            |---
            """.asUniverse().step() shouldEqual
            """
            |-x-
            |-x-
            |-x-
            """.asUniverse()
        """
            |-x-
            |xx-
            """.asUniverse().step() shouldEqual
            """
            |xx-
            |xx-
            """.asUniverse()
    }

    @Test fun `live cell with more than three live neighbours dies, as if by overpopulation`() {
        """
            |x-x
            |-x-
            |x-x
            """.asUniverse().step() shouldEqual
            """
            |-x-
            |x-x
            |-x-
            """.asUniverse()
    }

    @Test fun `dead cell with exactly three live neighbours becomes a live cell, as if by reproduction`() {
        """
            |x--
            |---
            |x-x
            """.asUniverse().step() shouldEqual
            """
            |---
            |-x-
            |---
            """.asUniverse()
    }

    data class Cell(val x: Int, val y: Int)

    data class Universe(private val liveCells: Set<Cell>) {
        fun step(): Universe {
            val survivedCells = liveCells.mapNotNull { cell ->
                val n = neighbours(cell).count { isAlive(it) }
                if (n == 2 || n == 3) cell else null
            }
            val newbornCells = liveCells.map { cell ->
                val deadCells = neighbours(cell).filterNot { isAlive(it) }
                deadCells.mapNotNull { deadCell ->
                    val n = neighbours(deadCell).count { isAlive(it) }
                    if (n == 3) deadCell else null
                }
            }.flatten()
            return Universe((survivedCells + newbornCells).toSet())
        }

        private fun isAlive(cell: Cell) = liveCells.contains(cell)

        private fun neighbours(cell: Cell): List<Cell> {
            val shifts = listOf(
                Pair(0, -1),
                Pair(-1, -1),
                Pair(-1, 0),
                Pair(-1, 1),
                Pair(0, 1),
                Pair(1, 1),
                Pair(1, 0),
                Pair(1, -1)
            )
            return shifts.map {
                Cell(cell.x + it.first, cell.y + it.second)
            }
        }
    }

    private fun String.asUniverse(): Universe {
        val cells = trim().trimMargin().split("\n").mapIndexed { y, line ->
            line.toCharArray().mapIndexed { x, char ->
                if (char == 'x') Cell(x, y) else null
            }.filterNotNull()
        }.flatten()
        return Universe(cells.toSet())
    }

}
