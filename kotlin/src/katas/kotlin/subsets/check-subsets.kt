package katas.kotlin.subsets

import katas.kotlin.pow
import katas.kotlin.shouldEqual
import org.junit.Test
import kotlin.random.Random

abstract class SubsetTests(private val subsetsOf: (Collection<Int>) -> Set<Set<Int>>) {
    @Test fun `find all subsets`() {
        subsetsOf(emptyList()) shouldEqual setOf(emptySet())
        subsetsOf(setOf(1)) shouldEqual setOf(setOf(1), emptySet())

        subsetsOf(setOf(1, 2)) shouldEqual setOf(
            emptySet(), setOf(1), setOf(2),
            setOf(1, 2)
        )

        subsetsOf(setOf(1, 2, 3)) shouldEqual setOf(
            emptySet(), setOf(1), setOf(2), setOf(3),
            setOf(1, 2), setOf(2, 3), setOf(1, 3),
            setOf(1, 2, 3)
        )
    }

    @Test fun `amount of subsets`() {
        subsetsOf(IntRange(0, 7).toList()).size shouldEqual 2.pow(8)

        val randomSet = Array(10, { Random.nextInt(0, 99) }).toSet()
        subsetsOf(randomSet).size shouldEqual 2.pow(randomSet.size)
    }
}