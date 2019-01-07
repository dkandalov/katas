package katas.kotlin.sort

import io.kotlintest.fail
import kotlincommon.listOfInts
import kotlincommon.permutations
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import kotlin.random.Random

abstract class SortingTests(private val sort: (List<Int>) -> List<Int>) {
    @Test fun `trivial examples`() {
        sort(emptyList()) shouldEqual emptyList()
        sort(listOf(1)) shouldEqual listOf(1)
        sort(listOf(1, 1)) shouldEqual listOf(1, 1)
    }

    @Test fun `basic examples`() {
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
    }

    @Test fun `sort random lists`() {
        val random = Random(seed = Random.nextInt().printed("seed="))

        // Odd size list
        random.listOfInts(size = 5, valuesRange = 0..5).printed().let {
            it.permutations().forEach { list ->
                sort(list).isSorted()
            }
        }
        // Even size list
        random.listOfInts(size = 6, valuesRange = 0..6).printed().let {
            it.permutations().forEach { list ->
                sort(list).isSorted()
            }
        }
    }

    private fun <T : Comparable<T>> List<T>.isSorted() {
        windowed(2).forEach { (item1, item2) ->
            if (item1 > item2) fail(
                "List $this is not sorted\n" +
                "because item $item1 was greater than $item2"
            )
        }
    }
}
