package katas.kotlin.skiena.combinatorial_search

import katas.kotlin.skiena.graphs.Edge
import katas.kotlin.skiena.graphs.Graph
import katas.kotlin.skiena.graphs.UnweightedGraphs
import kotlincommon.doesNotContain
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

    data class SubsetsResult(override val value: List<List<Int>> = emptyList()): Result<List<Int>> {
        override operator fun plus(solution: Solution<List<Int>>) = copy(value = value + listOf(solution.value))
        override operator fun plus(that: Result<List<Int>>) = SubsetsResult(value + that.value)
    }

    private fun subsets2(list: List<Int>): List<List<Int>> =
        backtrack(Subset(list), SubsetsResult()).value

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

    data class Paths(override val value: List<List<Int>> = emptyList()): Result<List<Int>> {
        override fun plus(solution: Solution<List<Int>>) = copy(value = value + listOf(solution.value))
        override fun plus(that: Result<List<Int>>) = copy(value = value + that.value)
    }

    private fun Graph<Int>.findAllPaths(from: Int, to: Int): List<List<Int>> {
        return backtrack(Path(this, from, to), Paths()).value
    }
}

class EightQueenTests {
    @Test fun `find queens positions`() {
        eightQueen(boardSize = 4) shouldEqual listOf(
            listOf(Queen(2, 0), Queen(0, 1), Queen(3, 2), Queen(1, 3)),
            listOf(Queen(1, 0), Queen(3, 1), Queen(0, 2), Queen(2, 3))
        )
        eightQueen(boardSize = 8).size shouldEqual 92
    }

    private data class Queen(val row: Int, val column: Int) {
        override fun toString() = "[$row,$column]"
    }

    private data class EightQueenSolution(
        val boardSize: Int,
        override val value: List<Queen> = emptyList(),
        val row: Int = 0,
        val column: Int = 0
    ): Solution<List<Queen>> {
        override fun isComplete() = value.size == boardSize && isValid()
        override fun hasNext() = !isComplete() && isValid() && column < boardSize
        override fun next() = copy(value = (value + Queen(row, column)), row = 0, column = column + 1)
        override fun skipNext() =
            if (row + 1 < boardSize) copy(row = row + 1)
            else copy(row = 0, column = column + 1)

        private fun isValid(): Boolean {
            if (value.isEmpty()) return true
            val queen = value.last()
            val queens = value.subList(0, value.lastIndex)
            val notOnTheSameLine = queens.none { it.row == queen.row || it.column == queen.column }
            val notOnTheSameDiagonal = queens.none {
                Math.abs(it.row - queen.row) == Math.abs(it.column - queen.column)
            }
            return notOnTheSameLine && notOnTheSameDiagonal
        }
    }

    private data class EightQueenResult(override val value: List<List<Queen>> = emptyList()): Result<List<Queen>> {
        override operator fun plus(solution: Solution<List<Queen>>): Result<List<Queen>> = copy(value = value + listOf(solution.value))
        override operator fun plus(that: Result<List<Queen>>): Result<List<Queen>> = copy(value = value + that.value)
    }

    private fun eightQueen(boardSize: Int): List<List<Queen>> {
        return backtrack(EightQueenSolution(boardSize), EightQueenResult()).value
    }
}

interface Solution<T> {
    val value: T
    fun isComplete(): Boolean
    fun hasNext(): Boolean
    fun next(): Solution<T>
    fun skipNext(): Solution<T>
}

interface Result<T> {
    val value: List<T>
    operator fun plus(solution: Solution<T>): Result<T>
    operator fun plus(that: Result<T>): Result<T>
}

fun <T> backtrack(solution: Solution<T>, emptyResult: Result<T>): Result<T> {
    return if (solution.hasNext()) {
        backtrack(solution.skipNext(), emptyResult) +
            backtrack(solution.next(), emptyResult)
    } else {
        if (solution.isComplete()) emptyResult + solution
        else emptyResult
    }
}
