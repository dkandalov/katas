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
        val list: List<Int>,
        override val value: List<Int> = emptyList(),
        val count: Int = 0
    ) : Solution<List<Int>> {
        override fun hasNext() = count < list.size
        override fun skipNext() = copy(count = count + 1)
        override fun next() = copy(value = value + list[count], count = count + 1)
        override fun isComplete() = true
    }

    private fun subsets2(list: List<Int>): List<List<Int>> =
        backtrack(Subset(list))

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

        override fun hasNext(): Boolean = nextEdge != null && !isComplete()
        override fun skipNext() = copy(skipped = skipped + nextEdge!!)
        override fun next() = copy(value = value + nextEdge!!.to)
        override fun isComplete() = value.last() == to
    }

    private fun Graph<Int>.findAllPaths(from: Int, to: Int): List<List<Int>> {
        return backtrack(Path(this, from, to))
    }
}


class EightQueenTests {
    @Test fun `find queens positions`() {
        eightQueen(boardSize = 4) shouldEqual listOf(
            listOf(Queen(2, 0), Queen(0, 1), Queen(3, 2), Queen(1, 3)),
            listOf(Queen(1, 0), Queen(3, 1), Queen(0, 2), Queen(2, 3))
        )
        eightQueen(boardSize = 8).let {
            it.take(5).forEach { solution ->
                println(solution)
                println(solution.toBoardString(8))
                println()
            }
            it.size shouldEqual 92
        }
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
        override fun hasNext() = !isComplete() && column < boardSize && valid

        override fun skipNext() =
            if (row + 1 < boardSize) copy(row = row + 1)
            else copy(row = 0, column = column + 1)

        override fun next() = Queen(row, column).let { queen ->
            copy(value = value + queen, row = 0, column = column + 1, valid = isValid(queen))
        }

        override fun isComplete() = value.size == boardSize && valid

        private fun isValid(queen: Queen): Boolean {
            val notOnTheSameLine = value.none { it.row == queen.row || it.column == queen.column }
            val notOnTheSameDiagonal = value.none {
                Math.abs(it.row - queen.row) == Math.abs(it.column - queen.column)
            }
            return notOnTheSameLine && notOnTheSameDiagonal
        }
    }

    private fun eightQueen(boardSize: Int): List<List<Queen>> {
        return backtrack(EightQueenSolution(boardSize))
    }

    private fun List<Queen>.toBoardString(boardSize: Int) =
        0.until(boardSize).join("\n") { row ->
            0.until(boardSize).join("") { column ->
                if (contains(Queen(row, column))) "*" else "-"
            }
        }
}


class SudokuTests {
    private val sudokuBoard = Sudoku.parse("""
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
        """.trimMargin())

    @Test fun `find sudoku solutions`() {
        backtrack(sudokuBoard).forEach { it.printed() }
        // TODO
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

        listOf(3, 4, 5).forEach { squareIndicesAt(index = it) shouldEqual listOf(3, 4, 5, 12, 13, 14, 21, 22, 23) }
        listOf(6, 7, 8).forEach { squareIndicesAt(index = it) shouldEqual listOf(6, 7, 8, 15, 16, 17, 24, 25, 26) }
        listOf(78, 79, 80).forEach { squareIndicesAt(index = it) shouldEqual listOf(60, 61, 62, 69, 70, 71, 78, 79, 80) }
    }

    private data class Sudoku(
        override val value: List<Int>,
        val guess: Int = 0,
        val index: Int = value.indexOf(0)
    ) : Solution<List<Int>> {

        override fun hasNext() = !(index == value.lastIndex && guess == 9) && isValid()

        override fun next(): Solution<List<Int>> {
            val (newIndex, newGuess) = if (guess == 9) Pair(value.indexOf(0), 1) else Pair(index, guess + 1)
            val newValue = ArrayList(value).apply { set(newIndex, newGuess) }
            return copy(value = newValue, guess = newGuess, index = newIndex)
        }

        override fun skipNext(): Solution<List<Int>> {
            val (newIndex, newGuess) = if (guess == 9) Pair(value.indexOf(0), 1) else Pair(index, guess + 1)
            return copy(guess = newGuess, index = newIndex)
        }

        override fun isComplete() = value.doesNotContain(0) && isValid()

        private fun isValid(): Boolean {
            if (value.isEmpty()) return true
            return rowIndicesAt(index).count { it == value[index] } <= 1 &&
                columnIndicesAt(index).count { it == value[index] } <= 1 &&
                squareIndicesAt(index).count { it == value[index] } <= 1
        }

        companion object {
            fun parse(s: String): Sudoku {
                val cells = s.replace(Regex("[|\\-+\n]"), "").mapTo(ArrayList()) { c ->
                    if (c == '.') 0 else c.toString().toInt()
                }
                return Sudoku(cells)
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

fun <T> backtrack(solution: Solution<T>): List<T> =
    when {
        solution.hasNext()    -> backtrack(solution.skipNext()) + backtrack(solution.next())
        solution.isComplete() -> listOf(solution.value)
        else                  -> emptyList()
    }
