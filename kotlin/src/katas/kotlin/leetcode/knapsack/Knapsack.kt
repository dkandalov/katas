package katas.kotlin.leetcode.knapsack

import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test


/**
 * Guided by Priyo based on
 * https://medium.com/@fabianterh/how-to-solve-the-knapsack-problem-with-dynamic-programming-eb88c706d3cf
 */
class KnapsackTests {
    @Test fun `find maximum amount of meetings fitting hours`() {
        optimise(setOf(Meeting(1)), hours = 0) shouldEqual emptySet()
        optimise(setOf(Meeting(1)), hours = 1) shouldEqual setOf(Meeting(1))
        optimise(setOf(Meeting(2)), hours = 1) shouldEqual emptySet()
        optimise(setOf(Meeting(2), Meeting(4), Meeting(1), Meeting(6)), hours = 8) shouldEqual
            setOf(Meeting(1), Meeting(2), Meeting(4))
    }

    @Test fun `find meetings that fill hours as close to maximum as possible`() {
        optimise2(setOf(Meeting(2), Meeting(4), Meeting(1), Meeting(6)), hours = 8) shouldEqual listOf(
            setOf(Meeting(2), Meeting(6))
        )

        optimise2(setOf(Meeting(4), Meeting(5), Meeting(6), Meeting(9)), hours = 9) shouldEqual listOf(
            setOf(Meeting(9)), setOf(Meeting(4), Meeting(5))
        )
        optimise2(setOf(Meeting(2), Meeting(4), Meeting(5), Meeting(6)), hours = 9) shouldEqual listOf(
            setOf(Meeting(4), Meeting(5))
        )
    }

    @Test fun `caching results`() {
        findMaxValue(
            maxCapacity = 10,
            items = listOf(
                Item(size = 5, value = 10),
                Item(size = 4, value = 40),
                Item(size = 6, value = 30),
                Item(size = 3, value = 50)
            )
        ).let { (maxValues, maxValue) ->
            maxValues.joinToString("\n") { it.toList().toString() } shouldEqual """
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                [0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10]
                [0, 0, 0, 0, 40, 40, 40, 40, 40, 50, 50]
                [0, 0, 0, 0, 40, 40, 40, 40, 40, 50, 70]
                [0, 0, 0, 50, 50, 50, 50, 90, 90, 90, 90]
            """.trimIndent()
            maxValue shouldEqual 90
        }

        findMaxValue(
            maxCapacity = 9,
            items = listOf(
                Item(size = 5, value = 1),
                Item(size = 4, value = 1),
                Item(size = 8, value = 1)
            )
        ).let { (maxValues, maxValue) ->
            maxValues.joinToString("\n") { it.toList().toString() }.printed()/* shouldEqual """
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                [0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10]
                [0, 0, 0, 0, 40, 40, 40, 40, 40, 50, 50]
                [0, 0, 0, 0, 40, 40, 40, 40, 40, 50, 70]
                [0, 0, 0, 50, 50, 50, 50, 90, 90, 90, 90]
            """.trimIndent()*/
//            maxValue shouldEqual 90
        }
    }
}

private data class Item(val size: Int, val value: Int)

private fun findMaxValue(maxCapacity: Int, items: List<Item>): Pair<Array<IntArray>, Int> {
    val maxValues = Array(items.size + 1) { IntArray(maxCapacity + 1) }
    items.forEachIndexed { itemIndex, (weight, value) ->
        (1..maxCapacity).forEach { capacity ->
            val maxValueWithoutItem = maxValues[itemIndex][capacity]
            val maxValueWithItem = if (capacity < weight) 0 else {
                val remainingCapacity = capacity - weight
                value + maxValues[itemIndex][remainingCapacity]
            }
            maxValues[itemIndex + 1][capacity] = maxOf(maxValueWithoutItem, maxValueWithItem)
        }
    }
    return Pair(maxValues, maxValues[items.size][maxCapacity])
}

private fun optimise2(meetings: Set<Meeting>, hours: Int): List<Set<Meeting>> {
    fun Set<Meeting>.duration() = sumBy { it.hours }

    val bestMeetings = doOptimise2(meetings, hours).sortedBy { -it.duration() }
    return bestMeetings
        .takeWhile { it.duration() == bestMeetings.first().duration() }
        .sortedBy { it.size }
}

private fun doOptimise2(meetings: Set<Meeting>, hours: Int): List<Set<Meeting>> {
    if (hours < 0) return emptyList()
    if (meetings.isEmpty()) return listOf(emptySet())
    val meeting = meetings.last()
    return doOptimise2(meetings - meeting, hours) +
        doOptimise2(meetings - meeting, hours - meeting.hours).map { it + meeting }
}

private fun optimise(meetings: Set<Meeting>, hours: Int): Set<Meeting> {
    val sorted = meetings.sortedBy { it.hours }.toMutableList()
    var hoursLeft = hours
    val result = LinkedHashSet<Meeting>()
    while (sorted.isNotEmpty()) {
        val meeting = sorted.removeAt(0)
        hoursLeft -= meeting.hours
        if (hoursLeft < 0) break
        result.add(meeting)
    }
    return result
}

private data class Meeting(val hours: Int) {
    override fun toString() = hours.toString()
}

