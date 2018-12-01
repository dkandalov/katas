package katas.kotlin.sort

import io.kotlintest.fail
import katas.kotlin.shouldEqual
import kotlincommon.permutations
import kotlin.random.Random

fun checkSortFunction(sort: (List<Int>) -> List<Int>) {
    sort(emptyList()) shouldEqual emptyList()
    sort(listOf(1)) shouldEqual listOf(1)

    sort(listOf(1, 2)) shouldEqual listOf(1, 2)
    sort(listOf(2, 1)) shouldEqual listOf(1, 2)

    sort(listOf(1, 2, 3)) shouldEqual listOf(1, 2, 3)
    sort(listOf(1, 3, 2)) shouldEqual listOf(1, 2, 3)
    sort(listOf(2, 1, 3)) shouldEqual listOf(1, 2, 3)
    sort(listOf(2, 3, 1)) shouldEqual listOf(1, 2, 3)
    sort(listOf(3, 1, 2)) shouldEqual listOf(1, 2, 3)
    sort(listOf(3, 2, 1)) shouldEqual listOf(1, 2, 3)

    // List with duplicates
    listOf(1, 2, 2, 3).permutations().forEach { list ->
        sort(list) shouldEqual listOf(1, 2, 2, 3)
    }

    // Odd size list
    Array(5, { Random.nextInt(0, 99) }).toList().let {
        it.permutations().forEach { list ->
            sort(list).isSorted()
        }
    }

    // Even size list
    Array(6, { Random.nextInt(0, 99) }).toList().let {
        it.permutations().forEach { list ->
            sort(list).isSorted()
        }
    }
}

private fun <T : Comparable<T>> List<T>.isSorted() {
    windowed(2).forEach { (item1, item2) ->
        if (item1 > item2) fail("Item $item1 was greater than $item2 in list: $this")
    }
}
