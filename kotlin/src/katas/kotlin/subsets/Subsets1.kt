package katas.kotlin.subsets

import katas.kotlin.shouldEqual
import org.junit.Test

class Subsets1 : SubsetTests({ it.subsets() }) {
    @Test fun `generate indices of subset elements`() {
        generateIndices(3).toSet() shouldEqual setOf(
            emptySet(),
            setOf(0),
            setOf(0, 1),
            setOf(0, 1, 2),
            setOf(0, 2),
            setOf(1),
            setOf(1, 2),
            setOf(2)
        )
    }

    companion object {
        private fun <E> Collection<E>.subsets(): Set<Set<E>> {
            return generateIndices(size).mapTo(HashSet<Set<E>>()) { indices ->
                this.filterIndexedTo(HashSet()) { index, _ ->
                    indices.contains(index)
                }
            }
        }

        private fun generateIndices(size: Int, from: Int = 0, indices: Set<Int> = emptySet()): Sequence<Set<Int>> {
            return sequence {
                yield(emptySet())
                from.until(size).forEach { i ->
                    yield(indices + i)
                    yieldAll(generateIndices(size, i + 1, indices + i))
                }
            }
        }
    }
}
