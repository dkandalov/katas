@file:Suppress("FunctionName")

import kotlin.test.Test
import kotlin.test.assertEquals


fun main(args: Array<String>) {
    println("hello")
    konan.test.main(args)
}

class KnapsackTests {
    private fun firstFit(values: Collection<Int>, target: Int): List<Int> {
        if (values.isEmpty()) return emptyList()
        val first = values.first()
        return if (first > target) firstFit(values - first, target)
        else listOf(first) + firstFit(values - first, target - first)
    }

    private fun bestFit(values: Collection<Int>, target: Int): List<Int> {
        return firstFit(values.sorted(), target)
    }

    private fun reverseBestFit(values: Collection<Int>, target: Int): List<Int> {
        return firstFit(values.sorted().reversed(), target)
    }

    @Test fun counterexamples() {
        val s = setOf(1, 2, 3, 5, 9, 10)

        firstFit(s, target = 23).let {
            assertEquals(it, listOf(1, 2, 3, 5, 9))
            assertEquals(it.sum(), 20)
        }
        bestFit(s, target = 23).let {
            assertEquals(it, listOf(1, 2, 3, 5, 9))
            assertEquals(it.sum(), 20)
        }

        reverseBestFit(setOf(3, 5, 7, 10), target = 18).let {
            assertEquals(it, listOf(10, 7))
            assertEquals(it.sum(), 17)
        }
    }
}