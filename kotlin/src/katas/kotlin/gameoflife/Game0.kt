package katas.kotlin.gameoflife

import datsok.shouldEqual
import org.junit.Test

class Game0 {
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
            |xxx
            """.asUniverse().step() shouldEqual
            """
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
            |x--
            |xx-
            |xxx
            """.asUniverse().step() shouldEqual
            """
            |xx-
            |--x
            |x-x
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

    private fun String.asUniverse(): List<String> = trim().trimMargin().split("\n")

    private fun List<String>.step(): List<String> {
        return mapIndexed { rowIndex, row ->
            row.mapIndexed { columnIndex, cell ->
                val neighbours = neighboursOf(rowIndex, columnIndex)
                when {
                    cell == 'x' && neighbours < 2 -> '-'
                    cell == 'x' && neighbours > 3 -> '-'
                    cell == '-' && neighbours == 3 -> 'x'
                    else -> cell
                }
            }.joinToString("")
        }
    }

    private fun List<String>.neighboursOf(rowIndex: Int, columnIndex: Int): Int {
        val positions = listOf(
            Pair(rowIndex, columnIndex - 1),
            Pair(rowIndex - 1, columnIndex - 1),
            Pair(rowIndex - 1, columnIndex),
            Pair(rowIndex - 1, columnIndex + 1),
            Pair(rowIndex, columnIndex + 1),
            Pair(rowIndex + 1, columnIndex + 1),
            Pair(rowIndex + 1, columnIndex),
            Pair(rowIndex + 1, columnIndex - 1)
        )
        return positions.sumBy { (y, x) ->
            if (y < 0 || y >= this.size) 0
            else if (x < 0 || x >= this[y].length) 0
            else if (this[y][x] == 'x') 1
            else 0
        }
    }
}
