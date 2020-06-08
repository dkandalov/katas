package katas.kotlin.leetcode.optimal_utilisation

import datsok.shouldEqual
import org.junit.Test

class OptimalUtilisationTests {
    @Test fun examples() {
        findOptimalPairs(
            a = listOf(Item(1, 2), Item(2, 4), Item(3, 6)),
            b = listOf(Item(1, 2)),
            target = 7
        ) shouldEqual listOf(Pair(2, 1))

        findOptimalPairs(
            a = listOf(Item(1, 3), Item(2, 5), Item(3, 7), Item(4, 1)),
            b = listOf(Item(1, 2), Item(2, 3), Item(3, 4), Item(4, 5)),
            target = 10
        ) shouldEqual listOf(Pair(2, 4), Pair(3, 2))
    }
}

private fun findOptimalPairs(a: List<Item>, b: List<Item>, target: Int): List<Pair<Int, Int>> {
    val sortedB = b.sortedBy { it.value }
    var max = Int.MIN_VALUE
    val maxIds = ArrayList<Pair<Int, Int>>()
    a.forEach { item1 ->
        val targetB = Item(-1, target - item1.value)
        val i = sortedB.binarySearch(targetB, Comparator { o1: Item, o2: Item -> o1.value.compareTo(o2.value) })
        val item2 = if (i >= 0) {
            sortedB[i]
        } else {
            val j = -i - 2
            if (j < 0) null
            else sortedB[j]
        }

        if (item2 != null) {
            val sum = item1.value + item2.value
            if (sum <= target) {
                if (sum > max) {
                    max = sum
                    maxIds.clear()
                }
                if (sum >= max) {
                    maxIds.add(Pair(item1.id, item2.id))
                }
            }
        }
    }
    return maxIds
}

private data class Item(val id: Int, val value: Int)