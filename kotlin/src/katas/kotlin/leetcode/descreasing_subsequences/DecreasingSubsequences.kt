package katas.kotlin.leetcode.descreasing_subsequences

import nonstdlib.printed
import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/350233/Google-or-Summer-Intern-OA-2019-or-Decreasing-Subsequences
 */
class DecreasingSubsequencesTests {
    @Test fun `split array into strictly decreasing subsequences`() {
        split(arrayOf(5, 2, 4, 3, 1, 6)).printed() shouldEqual listOf(listOf(5, 2, 1), listOf(4, 3), listOf(6))
        split(arrayOf(2, 9, 12, 13, 4, 7, 6, 5, 10)).printed() shouldEqual listOf(
            listOf(2), listOf(9, 4), listOf(12, 7, 6, 5), listOf(13, 10)
        )
        split(arrayOf(1, 1, 1)).printed() shouldEqual listOf(listOf(1), listOf(1), listOf(1))

        leastSubsequences(5, 2, 4, 3, 1, 6) shouldEqual 3
        leastSubsequences(2, 9, 12, 13, 4, 7, 6, 5, 10) shouldEqual 4
        leastSubsequences(1, 1, 1) shouldEqual 3
    }
}

private fun leastSubsequences(vararg nums: Int): Int {
    val piles = IntArray(nums.size)
    var size = 0
    for (n in nums) {
        val pile = binarySearch(piles, size, n)
        piles[pile] = n
        if (pile == size) size++
    }
    return size
}

private fun binarySearch(nums: IntArray, size: Int, target: Int): Int {
    var from = 0
    var to = size
    while (from < to) {
        val mid = (from + to) / 2
        if (target < nums[mid]) to = mid
        else from = mid + 1
    }
    return from
}

private fun split(array: Array<Int>): List<List<Int>> {
    return allSubsequences(Solution(array.toList())).minByOrNull { it: Solution -> it.seqs.size }!!.seqs
}

private fun allSubsequences(solution: Solution): List<Solution> {
    if (solution.isComplete()) return listOf(solution)
    return solution.nextValidSteps()
        .flatMap { subSolution -> allSubsequences(subSolution) }
}

private data class Solution(private val list: List<Int>, val seqs: List<List<Int>> = emptyList()) {
    fun isComplete() = list.isEmpty()

    fun nextValidSteps(): List<Solution> {
        val head = list.first()
        val tail = list.drop(1)
        return seqs.mapIndexedNotNull { i, seq ->
            if (seq.last() <= head) null
            else {
                val updatedValue = ArrayList(seqs).also { it[i] = seq + head }
                Solution(tail, updatedValue)
            }
        } + Solution(tail, seqs + listOf(listOf(head)))
    }

    override fun toString() = "Solution=$seqs"
}

