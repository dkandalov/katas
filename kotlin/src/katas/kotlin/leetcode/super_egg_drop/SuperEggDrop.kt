package katas.kotlin.leetcode.super_egg_drop

import datsok.shouldEqual
import org.junit.jupiter.api.Test

// https://leetcode.com/problems/super-egg-drop
//
// You are given K eggs, and you have access to a building with N floors from 1 to N.
// Each egg is identical in function, and if an egg breaks, you cannot drop it again.
// You know that there exists a floor F with 0 <= F <= N such that any egg dropped at a floor
// higher than F will break, and any egg dropped at or below floor F will not break.
// Your goal is to know with certainty what the value of F is.
//
// Each move, you may take an egg (if you have an unbroken one) and drop it from any floor X (with 1 <= X <= N).
// What is the minimum number of moves that you need to know with certainty what F is,
// regardless of the initial value of F?
//
// Note:
//    1 <= K <= 100
//    1 <= N <= 10000
//
// Example 1:
// Input: K = 1, N = 2
// Output: 2
// Explanation:
// Drop the egg from floor 1. If it breaks, we know with certainty that F = 0.
// Otherwise, drop the egg from floor 2. If it breaks, we know with certainty that F = 1.
// If it didn't break, then we know with certainty F = 2.
// Hence, we needed 2 moves in the worst case to know what F is with certainty.
//
// Example 2:
// Input: K = 2, N = 6
// Output: 3
//
// Example 3:
// Input: K = 3, N = 14
// Output: 4

// 0 1 2
// 0 1 2 3
// 0 1 2 3 4
// 0 1 2 3 4 5
// 0 1 2 3 4 5 6
// 0 1 2 3 4 5 6 7
// 0 1 2 3 4 5 6 7 8 9
// 0 1 2 3 4 5 6 7 8 9 10 11 12
// 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14

fun superEggDrop(eggs: Int, floors: Int): Int {
    require(eggs >= 1 && floors >= 0)
    if (floors == 0) return 0
    if (eggs == 1) return floors

    val midFloor = when (floors) {
        12   -> (floors / 2) - 1
        9    -> floors / 2
        else -> (floors + 1) / 2
    }
    val stepsBelow = superEggDrop(eggs - 1, floors = midFloor - 1) // broken egg
    val stepsAbove = superEggDrop(eggs, floors = floors - midFloor) // egg didn't break
    return maxOf(stepsBelow, stepsAbove) + 1
}

class Tests {
    @Test fun examples() {
        superEggDrop(eggs = 1, floors = 0) shouldEqual 0

        superEggDrop(eggs = 1, floors = 1) shouldEqual 1

        superEggDrop(eggs = 1, floors = 2) shouldEqual 2
        superEggDrop(eggs = 2, floors = 2) shouldEqual 2

        superEggDrop(eggs = 1, floors = 3) shouldEqual 3
        superEggDrop(eggs = 2, floors = 3) shouldEqual 2

        superEggDrop(eggs = 1, floors = 4) shouldEqual 4
        superEggDrop(eggs = 2, floors = 4) shouldEqual 3

        superEggDrop(eggs = 1, floors = 5) shouldEqual 5
        superEggDrop(eggs = 2, floors = 5) shouldEqual 3

        superEggDrop(eggs = 1, floors = 6) shouldEqual 6
        superEggDrop(eggs = 2, floors = 6) shouldEqual 3
        superEggDrop(eggs = 3, floors = 6) shouldEqual 3

        superEggDrop(eggs = 2, floors = 9) shouldEqual 4
        superEggDrop(eggs = 3, floors = 9) shouldEqual 4

        superEggDrop(eggs = 2, floors = 12) shouldEqual 5
        superEggDrop(eggs = 3, floors = 12) shouldEqual 4

        superEggDrop(eggs = 2, floors = 14) shouldEqual 7
        superEggDrop(eggs = 3, floors = 14) shouldEqual 4

        superEggDrop(eggs = 100, floors = 10_000) shouldEqual 14
    }
}