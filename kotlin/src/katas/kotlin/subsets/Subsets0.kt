package katas.kotlin.subsets

import katas.kotlin.pow
import katas.kotlin.shouldEqual
import org.junit.Test
import kotlin.random.Random

class Subsets0 {
    @Test fun `find all subsets`() {
        emptyList<Int>().naiveSubsets() shouldEqual setOf(emptySet())
        setOf(1).naiveSubsets() shouldEqual setOf(setOf(1), emptySet())

        setOf(1, 2).naiveSubsets() shouldEqual setOf(
            emptySet(), setOf(1), setOf(2),
            setOf(1, 2)
        )

        setOf(1, 2, 3).naiveSubsets() shouldEqual setOf(
            emptySet(), setOf(1), setOf(2), setOf(3),
            setOf(1, 2), setOf(2, 3), setOf(1, 3),
            setOf(1, 2, 3)
        )
    }

    @Test fun `amount of subsets`() {
        IntRange(0, 7).toSet().naiveSubsets().size shouldEqual 2.pow(8)

        val randomSet = Array(Random.nextInt(5), { Random.nextInt(0, 99) }).toSet()
        randomSet.naiveSubsets().size shouldEqual 2.pow(randomSet.size)
    }
}

private fun <E> Collection<E>.naiveSubsets(): Set<Set<E>> {
    val subsets = flatMapTo(HashSet()) { item -> (this - item).naiveSubsets() }
    return setOf(this.toSet()) + subsets
}
