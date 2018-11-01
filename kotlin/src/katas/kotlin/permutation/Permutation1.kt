@file:Suppress("EXPERIMENTAL_FEATURE_WARNING", "ConvertTwoComparisonsToRangeCheck")

package katas.kotlin.permutation

import kotlincommon.join
import kotlincommon.printed
import org.junit.Test

/**
 * The original paper http://www.ams.org/journals/mcom/1963-17-083/S0025-5718-1963-0159764-2/S0025-5718-1963-0159764-2.pdf
 * Implementation is based on https://www.cut-the-knot.org/Curriculum/Combinatorics/JohnsonTrotter.shtml
 */
class Permutation1 {
    @Test fun `permutations of a list`() {
        validatePermutationsFunction { it.permutations().toList() }

        listOf(0, 1).permutations().toList().printed()
        listOf(0, 1, 2).permutations().toList().printed()
        "abcd".toCharArray().toList().permutations().toList().printed()
        0.until(10_000).toList().permutations().take(10).toList().printed()
    }

    companion object {
        private const val left = -1
        private const val right = 1

        private data class Index(val value: Int, var direction: Int) {
            fun inverseDirection() {
                direction = if (direction == left) right else left
            }
            override fun toString() = (if (direction == left) "<" else ">") + value
        }

        private fun <E> List<E>.permutations(): Sequence<List<E>> {
            val list = this
            return sequence {
                val indices = list.indices.mapTo(ArrayList()) { Index(it, left) }
                var maxIndex: Index? = Index(-1, left)

                while (maxIndex != null) {
                    indices.join().printed()
                    yield(indices.map { list[it.value] })

                    maxIndex = indices.filterIndexed { i, _ -> indices.isMobile(i) }.maxBy { it.value }
                    if (maxIndex != null) {
                        val i = indices.indexOf(maxIndex)
                        indices.swap(i, i + maxIndex.direction)
                        indices.filter { it.value > maxIndex.value }
                            .forEach { it.inverseDirection() }
                    }
                }
            }
        }

        private fun List<Index>.isMobile(i: Int): Boolean {
            val j = i + this[i].direction
            return (j >= 0 && j < size) && this[i].value > this[j].value
        }
        
        private fun <E> MutableList<E>.swap(i1: Int, i2: Int) {
            val tmp = this[i1]
            this[i1] = this[i2]
            this[i2] = tmp
        }
    }
}
