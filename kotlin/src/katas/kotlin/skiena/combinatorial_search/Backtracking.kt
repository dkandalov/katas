package katas.kotlin.skiena.combinatorial_search

import katas.kotlin.skiena.graphs.Edge
import katas.kotlin.skiena.graphs.Graph
import katas.kotlin.skiena.graphs.UnweightedGraphs
import kotlincommon.doesNotContain
import kotlincommon.join
import kotlincommon.test.shouldEqual
import org.junit.Test

class SubsetsTests {
    @Test fun `find all subsets`() {
        subsets(setOf(1, 2, 3)).toString() shouldEqual "[[1, 2, 3], [2, 3], [3], [], [2], [1, 3], [1], [1, 2]]"
        subsets2(listOf(1, 2, 3)).toString() shouldEqual "[[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]"
    }

    data class Subset(val list: List<Int>, override val value: List<Int> = emptyList(), val count: Int = 0): Solution<List<Int>> {
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
    ): Solution<List<Int>> {
        override fun isComplete() = value.last() == to
        override fun hasNext() = !isComplete() && findNextEdge() != null
        override fun next() = copy(value = value + findNextEdge()!!.to)
        override fun skipNext() = copy(skipped = skipped + findNextEdge()!!)
        private fun findNextEdge() = graph.edgesByVertex[value.last()]!!.find { value.doesNotContain(it.to) && skipped.doesNotContain(it) }
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
    ): Solution<List<Queen>> {
        override fun isComplete() = value.size == boardSize && valid
        override fun hasNext() = !isComplete() && column < boardSize && valid
        override fun next() = Queen(row, column).let { queen ->
            copy(value = value + queen, row = 0, column = column + 1, valid = isValid(queen))
        }

        override fun skipNext() =
            if (row + 1 < boardSize) copy(row = row + 1)
            else copy(row = 0, column = column + 1)

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
                if (this@toBoardString.contains(Queen(row, column))) "*" else "-"
            }
        }
}

interface Solution<T> {
    val value: T
    fun isComplete(): Boolean
    fun hasNext(): Boolean
    fun next(): Solution<T>
    fun skipNext(): Solution<T>
}

fun <T> backtrack(solution: Solution<T>): List<T> =
    when {
        solution.hasNext()    -> backtrack(solution.skipNext()) + backtrack(solution.next())
        solution.isComplete() -> listOf(solution.value)
        else                  -> emptyList()
    }
