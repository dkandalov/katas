package katas.kotlin.leetcode.longest_string_without_consequetive_chars

import datsok.shouldEqual
import org.junit.Test
import java.util.*

/**
 * https://leetcode.com/discuss/interview-question/330356/Amazon-or-Online-Assessment-2019-or-Longest-string-without-3-consecutive-characters
 */
class LongestStringTests {
    @Test fun examples() {
        findMaxLengthString(a = 0, b = 0, c = 0) shouldEqual ""
        findMaxLengthString(a = 1, b = 0, c = 0) shouldEqual "a"
        findMaxLengthString(a = 0, b = 1, c = 0) shouldEqual "b"
        findMaxLengthString(a = 0, b = 0, c = 1) shouldEqual "c"
        findMaxLengthString(a = 3, b = 0, c = 0) shouldEqual "aa"

        findMaxLengthString(a = 1, b = 1, c = 1) shouldEqual "acb"
        findMaxLengthString(a = 2, b = 1, c = 1) shouldEqual "acab"

        findMaxLengthString(a = 1, b = 1, c = 6) shouldEqual "ccaccbcc"
        findMaxLengthString(a = 1, b = 2, c = 3) shouldEqual "cbcbca"
    }
}

private fun findMaxLengthString(a: Int, b: Int, c: Int): String {
    val heap = PriorityQueue<Pair<Int, Char>>(Comparator { o1, o2 -> -o1.first.compareTo(o2.first) })
    if (a > 0) heap.add(Pair(a, 'a'))
    if (b > 0) heap.add(Pair(b, 'b'))
    if (c > 0) heap.add(Pair(c, 'c'))

    var onHold: Pair<Int, Char>? = null
    var result = ""
    while (heap.isNotEmpty()) {
        val (count, char) = heap.remove()

        result += char

        if (onHold != null) {
            heap.add(onHold)
            onHold = null
        }

        val updatedCount = count - 1
        if (updatedCount > 0) {
            if (result.length >= 2 && result[result.length - 2] == char) {
                onHold = Pair(updatedCount, char)
            } else {
                heap.add(Pair(updatedCount, char))
            }
        }
    }
    return result
}

private fun findMaxLengthString_(a: Int, b: Int, c: Int): String {
    fun findMaxLengthString(solution: Solution): List<Solution> {
        if (solution.isComplete()) return listOf(solution)
        return solution.nextSteps()
            .filter { it.isValid() }
            .flatMap { findMaxLengthString(it) }
    }

    return findMaxLengthString(Solution(List(a) { 'a' }, List(b) { 'b' }, List(c) { 'c' }))
        .maxByOrNull { it.value.length }!!.value
}

private data class Solution(val a: List<Char>, val b: List<Char>, val c: List<Char>, val value: String = "") {
    fun isComplete() = a.isEmpty() && b.isEmpty() && c.isEmpty()
    fun isValid() = value.length < 3 || value.takeLast(3).toSet().size > 1
    fun nextSteps(): List<Solution> {
        return listOfNotNull(
            if (a.isNotEmpty()) Solution(a.drop(1), b, c, value + a.first()) else null,
            if (b.isNotEmpty()) Solution(a, b.drop(1), c, value + b.first()) else null,
            if (c.isNotEmpty()) Solution(a, b, c.drop(1), value + c.first()) else null
        )
    }
}