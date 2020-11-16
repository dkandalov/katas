package katas.kotlin.gameoflife.game1

import datsok.shouldEqual
import org.junit.Test


class Game1 {
    @Test fun `empty universe`() {
        """
        -
        """.parse().step() shouldEqual
            """
        |-
        """.parse()
    }

    @Test fun `live cell with fewer than two live neighbours dies, as if caused by underpopulation`() {
        """
        |o
        """.parse().step() shouldEqual
            """
        |-
        """.parse()

        """
        |oo
        """.parse().step() shouldEqual
            """
        |--
        """.parse()
    }

    @Test fun `blinker performance`() {
        var universe = listOf(
            "---",
            "ooo",
            "---"
        ).parse()
        universe = Universe(
            universe.liveCells +
                (1..100).flatMap { n ->
                    universe.liveCells.map { Cell(it.x + 10 * n, it.y + 10 * n) }
                }
        )

        repeat(times = 1000) {
            universe = universe.step()
        }
    }

    @Test fun `glider performance`() {
        var universe = listOf(
            "-o-",
            "--o",
            "ooo",
        ).parse()
        universe = Universe(
            universe.liveCells +
                (1..100).flatMap { n ->
                    universe.liveCells.map { Cell(it.x + 10 * n, it.y + 10 * n) }
                }
        )

        repeat(times = 1000) {
            universe = universe.step()
        }
    }

    @Test fun `glider moving`() {
        var universe = listOf(
            "-o-",
            "--o",
            "ooo",
        ).parse()

        repeat(times = 100) {
            universe = universe.step()
            println(universe.toPrintableString(0..10, 0..20) + "\n")
            Thread.sleep(500)
        }
    }

    @Test fun `live cell with two or three live neighbours lives on to the next generation`() {
        listOf(
            "---",
            "ooo",
            "---"
        ).parse().step() shouldEqual listOf(
            "-o-",
            "-o-",
            "-o-",
        ).parse()

        """
        |-o-
        |oo-
        """.parse().step() shouldEqual
            """
        |oo-
        |oo-
        """.parse()
    }

    @Test fun `live cell with more than three live neighbours dies, as if by overpopulation`() {
        """
        |o-o
        |-o-
        |o-o
        """.parse().step() shouldEqual
            """
        |-o-
        |o-o
        |-o-
        """.parse()
    }

    @Test fun `dead cell with exactly three live neighbours becomes a live cell, as if by reproduction`() {
        """
        |o--
        |---
        |o-o
        """.parse().step() shouldEqual
            """
        |---
        |-o-
        |---
        """.parse()
    }
}

data class Cell(val x: Int, val y: Int) {
    val neighbours: List<Cell> by lazy {
        listOf(
            Pair(-1, -1), Pair(-1, 0), Pair(-1, 1),
            Pair(0, -1), /*Pair(0, 0),*/ Pair(0, 1),
            Pair(1, -1),  Pair(1, 0),  Pair(1, 1),
        ).map {
            Cell(x + it.first, y + it.second)
        }
    }

    fun neighbours(): List<Cell> = neighbours

/*
    fun neighbours(): Sequence<Cell> = sequence {
        (-1..1).forEach { xShift ->
            (-1..1).forEach { yShift ->
                if (xShift != 0 || yShift != 0) {
                    yield(Cell(x + xShift, y + yShift))
                }
            }
        }
    }
*/
}

data class Universe(val liveCells: Set<Cell>) {
    fun step(): Universe {
        val cells = (liveCells + liveCells.flatMap { it.neighbours() })
            .filterTo(LinkedHashSet()) { cell ->
                val neighbourCount = cell.liveNeighbourCount()
                if (cell !in liveCells) neighbourCount == 3
                else neighbourCount == 2 || neighbourCount == 3
            }
        return Universe(cells)
    }

    fun step_(): Universe {
        val survivedCells = liveCells.filter { cell ->
            val n = cell.liveNeighbourCount()
            n == 2 || n == 3
        }
        val newbornCells = liveCells
            .flatMap { it.neighbours() }
            .filter { cell ->
                cell !in liveCells && cell.liveNeighbourCount() == 3
            }

        return Universe((survivedCells + newbornCells).toSet())
    }

    private fun Cell.liveNeighbourCount(): Int =
        neighbours.count { it in liveCells }

}

private fun Universe.toPrintableString(): String {
    return this.toPrintableString(
        xRange = liveCells.minOf { it.x }..liveCells.maxOf { it.x },
        yRange = liveCells.minOf { it.y }..liveCells.maxOf { it.y }
    )
}

private fun Universe.toPrintableString(xRange: IntRange, yRange: IntRange): String {
    return buildString {
        xRange.forEach { x ->
            yRange.forEach { y ->
                if (Cell(x, y) in liveCells) append("o") else append("-")
            }
            appendLine()
        }
    }
}

private /*inline*/ operator fun IntRange.times(that: IntRange) =
    this.flatMap { i ->
        that.map { j ->
            Pair(i, j)
        }
    }

private fun List<String>.parse(): Universe {
    val cells = mapIndexed { y, line ->
        line.toCharArray().mapIndexed { x, char ->
            if (char == 'o') Cell(x, y) else null
        }.filterNotNull()
    }.flatten()
    return Universe(cells.toSet())
}

private fun String.parse(): Universe {
    val cells = trimIndent().trim().trimMargin().split("\n").mapIndexed { y, line ->
        line.toCharArray().mapIndexed { x, char ->
            if (char == 'o') Cell(x, y) else null
        }.filterNotNull()
    }.flatten()
    return Universe(cells.toSet())
}
