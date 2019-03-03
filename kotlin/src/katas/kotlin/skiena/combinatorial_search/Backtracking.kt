package katas.kotlin.skiena.combinatorial_search

import katas.kotlin.skiena.combinatorial_search.SudokuTests.Sudoku.Companion.columnIndicesAt
import katas.kotlin.skiena.combinatorial_search.SudokuTests.Sudoku.Companion.rowIndicesAt
import katas.kotlin.skiena.combinatorial_search.SudokuTests.Sudoku.Companion.squareIndicesAt
import katas.kotlin.skiena.graphs.Edge
import katas.kotlin.skiena.graphs.Graph
import katas.kotlin.skiena.graphs.UnweightedGraphs
import kotlincommon.doesNotContain
import kotlincommon.join
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.ArrayList

class SubsetsTests {
    @Test fun `find all subsets`() {
        subsets(setOf(1, 2, 3)).toString() shouldEqual "[[1, 2, 3], [2, 3], [3], [], [2], [1, 3], [1], [1, 2]]"
        subsets2(listOf(1, 2, 3)).toString() shouldEqual "[[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]"
    }

    data class Subset(
        private val list: List<Int>,
        override val value: List<Int> = emptyList(),
        private val index: Int = 0
    ) : Solution<List<Int>> {
        override fun hasNext() = index < list.size
        override fun skipNext() = copy(index = index + 1)
        override fun next() = copy(value = value + list[index], index = index + 1)
        override fun isComplete() = !hasNext()
    }

    private fun subsets2(list: List<Int>): List<List<Int>> =
        backtrack(Subset(list)).map { it.value }

    fun subsets(set: Set<Int>): Set<Set<Int>> {
        return setOf(set) + set.flatMap { subsets(set - it) }
    }
}


class AllPathsTests {
    @Test fun `find all paths in a graph`() {
        UnweightedGraphs.linearGraph.let {
            it.findAllPaths(from = 1, to = 1).toString() shouldEqual "[[1]]"
            it.findAllPaths(from = 1, to = 2).toString() shouldEqual "[[1, 2]]"
            it.findAllPaths(from = 1, to = 3).toString() shouldEqual "[[1, 2, 3]]"
        }
        UnweightedGraphs.disconnectedGraph.findAllPaths(from = 1, to = 4).toString() shouldEqual "[]"
        UnweightedGraphs.diamondGraph.findAllPaths(from = 1, to = 3).toString() shouldEqual "[[1, 4, 3], [1, 2, 3]]"
        UnweightedGraphs.meshGraph.findAllPaths(from = 1, to = 3).toString() shouldEqual "[[1, 4, 3], [1, 4, 2, 3], [1, 3], [1, 2, 4, 3], [1, 2, 3]]"
    }

    data class Path(
        private val graph: Graph<Int>,
        private val from: Int,
        private val to: Int,
        override val value: List<Int> = listOf(from),
        private val skipped: Set<Edge<Int>> = emptySet()
    ) : Solution<List<Int>> {
        private val nextEdge = graph.edgesByVertex[value.last()]!!.find { value.doesNotContain(it.to) && skipped.doesNotContain(it) }

        override fun hasNext() = nextEdge != null && !isComplete()
        override fun skipNext() = copy(skipped = skipped + nextEdge!!)
        override fun next() = copy(value = value + nextEdge!!.to)
        override fun isComplete() = value.last() == to
    }

    private fun Graph<Int>.findAllPaths(from: Int, to: Int): List<List<Int>> {
        return backtrack(Path(this, from, to)).map { it.value }
    }
}


class EightQueenTests {
    @Test fun `find queens positions on 4x4 board`() {
        eightQueen(boardSize = 4).map { it.toBoardString(4) } shouldEqual listOf(
            """|-*--
               |---*
               |*---
               |--*-
            """,
            """|--*-
               |*---
               |---*
               |-*--
            """
        ).map { it.trimMargin() }
    }

    @Test fun `find queens positions on 8x8 board`() {
        val solutions = eightQueen(boardSize = 8)
        val (solution1, solution2, solution3) = solutions.take(3).map { it.toBoardString(8) }

        solution1 shouldEqual """
                |--*-----
                |-----*--
                |---*----
                |-*------
                |-------*
                |----*---
                |------*-
                |*-------
            """.trimMargin()

        solution2 shouldEqual """
                |--*-----
                |----*---
                |-*------
                |-------*
                |-----*--
                |---*----
                |------*-
                |*-------
            """.trimMargin()

        solution3 shouldEqual """
                |----*---
                |-*------
                |---*----
                |------*-
                |--*-----
                |-------*
                |-----*--
                |*-------
            """.trimMargin()

        solutions.size shouldEqual 92
    }

    private data class Queen(val row: Int, val column: Int) {
        override fun toString() = "[$row,$column]"
    }

    private data class EightQueenSolution(
        val boardSize: Int,
        override val value: List<Queen> = emptyList(),
        val row: Int = 0,
        val column: Int = 0,
        val valid: Boolean = true
    ) : Solution<List<Queen>> {
        override fun hasNext() = !isComplete() && valid

        override fun skipNext() =
            if (row + 1 < boardSize) copy(row = row + 1)
            else copy(row = 0, column = column + 1, valid = false)

        override fun next() = Queen(row, column).let { queen ->
            copy(value = value + queen, row = 0, column = column + 1, valid = isValid(queen))
        }

        override fun isComplete() = boardSize == value.size && valid

        private fun isValid(queen: Queen): Boolean {
            val notOnTheSameLine = value.none { it.row == queen.row || it.column == queen.column }
            val notOnTheSameDiagonal = value.none {
                Math.abs(it.row - queen.row) == Math.abs(it.column - queen.column)
            }
            return notOnTheSameLine && notOnTheSameDiagonal
        }
    }

    private fun eightQueen(boardSize: Int): List<List<Queen>> {
        return backtrack(EightQueenSolution(boardSize)).map { it.value }
    }

    private fun List<Queen>.toBoardString(boardSize: Int) =
        0.until(boardSize).join("\n") { row ->
            0.until(boardSize).join("") { column ->
                if (contains(Queen(row, column))) "*" else "-"
            }
        }
}


class SudokuTests {
    private val sudokuBoardString = """
            |6..|...|2.3
            |...|4.3|8..
            |.3.|7..|..9
            |---+---+---
            |...|.2.|1..
            |49.|...|.65
            |..6|.9.|...
            |---+---+---
            |1..|..5|.8.
            |..9|6..|...
            |8.4|...|..2
        """.trimMargin()
    private val sudoku = Sudoku.parse(sudokuBoardString)

    @Test fun `find easy sudoku solutions`() {
        val solutions = backtrack(sudoku)
        solutions.forEach { it.printed("\n\n") }

        solutions.first() shouldEqual Sudoku.parse("""
            |648|951|273
            |927|463|851
            |531|782|649
            |---+---+---
            |785|326|194
            |492|817|365
            |316|594|728
            |---+---+---
            |173|245|986
            |259|638|417
            |864|179|532
        """.trimIndent())
    }

    @Test fun `sudoku solution steps`() {
        sudoku.hasNext() shouldEqual true

        sudoku.next().let {
            it.toString().lines().first() shouldEqual "61.|...|2.3"
            it.hasNext() shouldEqual true
        }
        sudoku.next().next().let {
            it.toString().lines().first() shouldEqual "611|...|2.3"
            it.hasNext() shouldEqual false
        }

        sudoku.skipNext().next().let {
            it.toString().lines().first() shouldEqual "62.|...|2.3"
            it.hasNext() shouldEqual false
        }
        sudoku.skipNext().skipNext().next().let {
            it.toString().lines().first() shouldEqual "63.|...|2.3"
            it.hasNext() shouldEqual false
        }
        sudoku.skipNext().skipNext().skipNext().next().let {
            it.toString().lines().first() shouldEqual "64.|...|2.3"
            it.hasNext() shouldEqual true
        }
    }

    @Test fun `sudoku conversion to string`() {
        sudoku.toString() shouldEqual sudokuBoardString
    }

    @Test fun `indices of sudoku rows`() {
        0.until(9).forEach { rowIndicesAt(index = it) shouldEqual listOf(0, 1, 2, 3, 4, 5, 6, 7, 8) }
        9.until(18).forEach { rowIndicesAt(index = it) shouldEqual listOf(9, 10, 11, 12, 13, 14, 15, 16, 17) }
        72.until(81).forEach { rowIndicesAt(index = it) shouldEqual listOf(72, 73, 74, 75, 76, 77, 78, 79, 80) }
    }

    @Test fun `indices of sudoku columns`() {
        0.until(9).map { it * 9 }.forEach { columnIndicesAt(index = it) shouldEqual listOf(0, 9, 18, 27, 36, 45, 54, 63, 72) }
        0.until(9).map { it * 9 + 1 }.forEach { columnIndicesAt(index = it) shouldEqual listOf(1, 10, 19, 28, 37, 46, 55, 64, 73) }
        0.until(9).map { it * 9 + 8 }.forEach { columnIndicesAt(index = it) shouldEqual listOf(8, 17, 26, 35, 44, 53, 62, 71, 80) }
    }

    @Test fun `indices of sudoku squares`() {
        listOf(0, 1, 2).forEach { squareIndicesAt(index = it) shouldEqual listOf(0, 1, 2, 9, 10, 11, 18, 19, 20) }
        listOf(9, 10, 11).forEach { squareIndicesAt(index = it) shouldEqual listOf(0, 1, 2, 9, 10, 11, 18, 19, 20) }
        listOf(18, 19, 20).forEach { squareIndicesAt(index = it) shouldEqual listOf(0, 1, 2, 9, 10, 11, 18, 19, 20) }

        listOf(3, 4, 5, 12, 13, 14, 21, 22, 23).forEach { squareIndicesAt(index = it) shouldEqual listOf(3, 4, 5, 12, 13, 14, 21, 22, 23) }
        listOf(6, 7, 8, 15, 16, 17, 24, 25, 26).forEach { squareIndicesAt(index = it) shouldEqual listOf(6, 7, 8, 15, 16, 17, 24, 25, 26) }
        listOf(60, 61, 62, 69, 70, 71, 78, 79, 80).forEach { squareIndicesAt(index = it) shouldEqual listOf(60, 61, 62, 69, 70, 71, 78, 79, 80) }
    }

    private data class Sudoku(
        override val value: List<Int>,
        private val guessIndex: Int = value.indexOf(0).let { if (it == -1) value.size else it },
        private val guess: Int = 1
    ) : Solution<List<Int>> {
        private val isValid = isValid()

        override fun hasNext() = guess <= 9 && guessIndex >= 0 && guessIndex < value.size && isValid

        override fun skipNext() = copy(guess = guess + 1)

        override fun next(): Solution<List<Int>> {
            val newValue = ArrayList(value).apply<ArrayList<Int>> { set(guessIndex, guess) }
            return Sudoku(value = newValue, guess = 1)
        }

        override fun isComplete() = isValid && value.doesNotContain(0)

        private fun isValid(): Boolean =
            0.until(guessIndex).all { index ->
                val n = value[index]
                rowIndicesAt(index).count { value[it] == n } == 1 &&
                    columnIndicesAt(index).count { value[it] == n } == 1 &&
                    squareIndicesAt(index).count { value[it] == n } == 1
            }

        override fun toString(): String {
            return value.windowed(size = 9, step = 9).mapIndexed { i, row ->
                val separator = if (i == 3 || i == 6) "---+---+---\n" else ""
                separator + row.windowed(size = 3, step = 3).joinToString("|") {
                    it.joinToString("").replace('0', '.')
                }
            }.joinToString("\n")
        }

        companion object {
            fun parse(s: String): Sudoku {
                val cells = s.replace(Regex("[|\\-+\n]"), "").mapTo(ArrayList()) { c ->
                    if (c == '.') 0 else c.toString().toInt()
                }
                return Sudoku(cells, guess = 1)
            }

            fun rowIndicesAt(index: Int): List<Int> {
                val result = ArrayList<Int>()
                val firstColumnInRow = index / 9 * 9
                firstColumnInRow.until(firstColumnInRow + 9).forEach { result.add(it) }
                return result
            }

            fun columnIndicesAt(index: Int): List<Int> {
                val result = ArrayList<Int>()
                val column = index.rem(9)
                0.until(9).map { it * 9 + column }.forEach { result.add(it) }
                return result
            }

            fun squareIndicesAt(index: Int): List<Int> {
                val result = ArrayList<Int>()
                val firstColumnInSquare = (index / 3 * 3).rem(9)
                val firstColumnInRow = index / (9 * 3) * (9 * 3)
                0.until(3).map { firstColumnInRow + it * 9 }.forEach { startOfRow ->
                    0.until(3).map { firstColumnInSquare + it }.forEach { colShift ->
                        result.add(startOfRow + colShift)
                    }
                }
                return result
            }
        }
    }
}


interface Solution<T> {
    val value: T
    fun hasNext(): Boolean
    fun skipNext(): Solution<T>
    fun next(): Solution<T>
    fun isComplete(): Boolean
}

fun <T> backtrack(solution: Solution<T>): List<Solution<T>> {
    val solutions = if (solution.hasNext()) backtrack(solution.skipNext()) + backtrack(solution.next()) else emptyList()
    return if (solution.isComplete()) solutions + solution else solutions
}
