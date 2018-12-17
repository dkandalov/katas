package katas.kotlin.subsets

import kotlincommon.pow
import kotlincommon.test.shouldEqual
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
        checkAmountOfSubsets(subsets = subsetsOf(IntRange(0, 7).toList()), setSize = 8)

        val randomSet = Array(10, { Random.nextInt(0, 99) }).toSet()
        checkAmountOfSubsets(subsets = subsetsOf(randomSet), setSize = randomSet.size)
    }

    private fun checkAmountOfSubsets(subsets: Set<Set<Int>>, setSize: Int) {
        subsets.size shouldEqual 2.pow(setSize)

        val expectedAmountOfSetsBySize = pascalTriangle(setSize + 1).last()
            .mapIndexed { index, it -> Pair(index, it) }.toMap()

        val actualAmountOfSetsBySize = subsets.fold(HashMap<Int, Int>()) { map, it ->
            map[it.size] = map.getOrDefault(it.size, 0) + 1
            map
        }
        actualAmountOfSetsBySize shouldEqual expectedAmountOfSetsBySize
    }
}