package katas.kotlin.skiena.combinatorial_search

import kotlincommon.test.shouldEqual

fun main() {
    subsets(setOf(1, 2, 3)).toString() shouldEqual "[[1, 2, 3], [2, 3], [3], [], [2], [1, 3], [1], [1, 2]]"
    subsets2(Input(listOf(1, 2, 3))).toString() shouldEqual "Result(value=[[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]])"
}

data class Input(val list: List<Int>, val count: Int = 0) {
    fun value() = list[count]
    fun next() = copy(count = count + 1)
    fun hasNext() = count < list.size
}

data class Solution(val value: List<Int> = emptyList()) {
    operator fun plus(input: Input) = Solution(value + input.value())
}

data class Result(val value: List<List<Int>> = ArrayList()) {
    operator fun plus(solution: Solution) = copy(value = value + setOf(solution.value))
    operator fun plus(that: Result) = Result(ArrayList(value + that.value))
}

fun subsets2(input: Input, solution: Solution = Solution()): Result {
    return if (input.hasNext()) {
        subsets2(input.next(), solution) +
        subsets2(input.next(), solution + input)
    } else {
        Result() + solution
    }
}

fun subsets(set: Set<Int>): Set<Set<Int>> {
    return setOf(set) + set.flatMap { subsets(set - it) }
}
