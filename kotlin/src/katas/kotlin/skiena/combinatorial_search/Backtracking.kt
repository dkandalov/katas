package katas.kotlin.skiena.combinatorial_search

import kotlincommon.test.shouldEqual

fun main() {
    subsets(setOf(1, 2, 3)).toString() shouldEqual "[[1, 2, 3], [2, 3], [3], [], [2], [1, 3], [1], [1, 2]]"
    subsets2(listOf(1, 2, 3)).toString() shouldEqual "[[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]"
}

interface Input<E> {
    fun step(): E
    fun next(): Input<E>
    fun hasNext(): Boolean
}

interface Solution<T, E> {
    val value: T
    operator fun plus(step: E): Solution<T, E>
}

interface Result<T, E> {
    val value: List<T>
    operator fun plus(solution: Solution<T, E>): Result<T, E>
    operator fun plus(that: Result<T, E>): Result<T, E>
}

data class SubsetsInput(val list: List<Int>, val count: Int = 0): Input<Int> {
    override fun step() = list[count]
    override fun next() = copy(count = count + 1)
    override fun hasNext() = count < list.size
}

data class SubsetsSolution(override val value: List<Int> = emptyList()): Solution<List<Int>, Int> {
    override operator fun plus(step: Int) = SubsetsSolution(value + step)
}

data class SubsetsResult(override val value: List<List<Int>> = ArrayList()): Result<List<Int>, Int> {
    override operator fun plus(solution: Solution<List<Int>, Int>) = copy(value = value + setOf(solution.value))
    override operator fun plus(that: Result<List<Int>, Int>) = SubsetsResult(ArrayList(value + that.value))
}

fun subsets2(list: List<Int>): List<List<Int>> =
    backtrack(SubsetsInput(list), SubsetsSolution(), { SubsetsResult() }).value

fun <T, E> backtrack(input: Input<E>, solution: Solution<T, E>, emptyResult: () -> Result<T, E>): Result<T, E> {
    return if (input.hasNext()) {
        backtrack(input.next(), solution, emptyResult) +
        backtrack(input.next(), solution + input.step(), emptyResult)
    } else {
        emptyResult() + solution
    }
}

fun subsets(set: Set<Int>): Set<Set<Int>> {
    return setOf(set) + set.flatMap { subsets(set - it) }
}
