package katas.kotlin.leetcode.adjust_salaries

import datsok.shouldEqual
import org.junit.Test

/**
 * Give an array of salaries. The total salary has a budget.
 * At the beginning, the total salary of employees is larger than the budget.
 * It is required to find the number k, and reduce all the salaries larger than k to k,
 * such that the total salary is exactly equal to the budget.
 *
 * https://leetcode.com/discuss/interview-question/351313/Google-or-Phone-Screen-or-Salary-Adjustment
 */
class AdjustSalariesTests {
    @Test fun `it works`() {
        adjustSalaries(arrayOf(10, 30, 20, 40), budget = 80) shouldEqual arrayOf(10, 20, 25, 25)
    }
}

private fun adjustSalaries(salaries: Array<Int>, budget: Int): Array<Int> {
    salaries.sort()
    var deficit = salaries.sum() - budget
    var maxIndex = salaries.size - 1
    while (deficit > 0) {
        maxIndex = findMaxIndex(salaries, maxIndex)
        val max = salaries[maxIndex]
        val maxCount = salaries.size - maxIndex
        val nextMax = salaries[maxIndex - 1]
        var space = (max - nextMax) * maxCount

        if (space < deficit) {
            deficit -= space
        } else {
            space = deficit
            deficit -= deficit
        }
        (maxIndex until salaries.size).forEach { i ->
            salaries[i] -= space / maxCount
        }
    }

    return salaries
}

private fun findMaxIndex(a: Array<Int>, from: Int): Int {
    var i = from
    var max = Int.MIN_VALUE
    var maxIndex = i
    while (i >= 0) {
        if (a[i] >= max) {
            max = a[i]
            maxIndex = i
        }
        i--
    }
    return maxIndex
}