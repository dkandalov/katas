package katas.kotlin.sort

import datsok.shouldEqual
import io.kotlintest.fail
import nonstdlib.listOfInts
import nonstdlib.permutations
import nonstdlib.printed
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
                sort(list).expectToBeSorted()
            }
        }
        // Even size list
        random.listOfInts(size = 6, valuesRange = 0..6).printed().let {
            it.permutations().forEach { list ->
                sort(list).expectToBeSorted()
            }
        }

        val list = random.listOfInts(sizeRange = 0..100).printed()
        sort(list).expectToBeSorted()
    }

    private fun <T : Comparable<T>> List<T>.expectToBeSorted() {
        windowed(2).forEach { (item1, item2) ->
            if (item1 > item2) fail(
                "List $this is not sorted\n" +
                "because item $item1 was greater than $item2"
            )
        }
    }
}
