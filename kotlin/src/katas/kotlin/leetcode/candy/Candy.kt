package katas.kotlin.leetcode.candy

import datsok.shouldEqual
import org.junit.jupiter.api.Test

//
// https://leetcode.com/problems/candy ✅
//
// There are N children standing in a line. Each child is assigned a rating value.
// You are giving candies to these children subjected to the following requirements:
//    - Each child must have at least one candy.
//    - Children with a higher rating get more candies than their neighbors.
// What is the minimum candies you must give?
//
// Example 1:
// Input: [1,0,2]
// Output: 5
// Explanation: You can allocate to the first, second and third child with 2, 1, 2 candies respectively.
//
// Example 2:
// Input: [1,2,2]
// Output: 4
// Explanation: You can allocate to the first, second and third child with 1, 2, 1 candies respectively.
//              The third child gets 1 candy because it satisfies the above two conditions.
//

// [0,8,5,3,0]
// [1,4,3,2,1]
// [1,4,3,2,1]

// [0,3,5,8,0]
// [1,2,3,4,1]

// [0,3,5,8,4,2,1]
// [1,2,3,4,3,2,1]

// [0,8,5,3,0,8,5,3,0]
// [1,4,3,2,1,4,3,2,1]

fun candy(ratings: IntArray): Int =
    candyPerChild_(ratings).sum()

private fun candyPerChild(ratings: IntArray): List<Int> =
    candyPerChild(Children(ratings.mapIndexed { index, rating -> Child(index, rating, 0) }))
        .map { it.candyAmount }

private fun candyPerChild(children: Children): Children {
    children.forEach { child -> child.candyAmount = 1 }
    (children + children.reversed()).forEach { child ->
        children.neighboursOf(child).forEach { neighbour ->
            if (isUnfairFor(child, neighbour)) child.candyAmount = neighbour.candyAmount + 1
        }
    }
    return children
}

private class Children(private val value: List<Child>) : Iterable<Child> by value {
    fun neighboursOf(child: Child) = listOfNotNull(
        (child.index - 1).let { if (it >= 0) value[it] else null },
        (child.index + 1).let { if (it <= value.lastIndex) value[it] else null }
    )
}

private fun isUnfairFor(child: Child, neighbour: Child) =
    child.rating > neighbour.rating && child.candyAmount <= neighbour.candyAmount

private data class Child(val index: Int, val rating: Int, var candyAmount: Int)


fun candyPerChild_(ratings: IntArray): List<Int> {
    val children = ratings.indices

    fun neighboursOf(child: Int) =
        listOf(child - 1, child + 1).filter { it in 0..children.last }

    val candies = MutableList(ratings.size) { 1 }
    (children + children.reversed()).forEach { child ->
        neighboursOf(child).forEach { neighbour ->
            if (ratings[child] > ratings[neighbour] && candies[child] <= candies[neighbour]) {
                candies[child] = candies[neighbour] + 1
            }
        }
    }
    return candies
}

class Tests {
    @Test fun `some examples`() {
        candyPerChild_(ratings = intArrayOf()) shouldEqual emptyList()
        candyPerChild_(ratings = intArrayOf(0)) shouldEqual listOf(1)
        candyPerChild_(ratings = intArrayOf(1)) shouldEqual listOf(1)
        candyPerChild_(ratings = intArrayOf(1, 0, 2)) shouldEqual listOf(2, 1, 2)
        candyPerChild_(ratings = intArrayOf(1, 2, 2)) shouldEqual listOf(1, 2, 1)
        candyPerChild_(ratings = intArrayOf(0, 3, 5, 8, 0)) shouldEqual listOf(1, 2, 3, 4, 1)
        candyPerChild_(ratings = intArrayOf(0, 8, 5, 3, 0)) shouldEqual listOf(1, 4, 3, 2, 1)
        candyPerChild_(ratings = intArrayOf(0, 3, 5, 8, 4, 2, 10)) shouldEqual listOf(1, 2, 3, 4, 2, 1, 2)
    }
}