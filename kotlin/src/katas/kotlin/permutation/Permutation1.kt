@file:Suppress("EXPERIMENTAL_FEATURE_WARNING", "ConvertTwoComparisonsToRangeCheck")

package katas.kotlin.permutation

import kotlincommon.printed
import kotlincommon.swap
import org.junit.Test

/**
 * The original paper http://www.ams.org/journals/mcom/1963-17-083/S0025-5718-1963-0159764-2/S0025-5718-1963-0159764-2.pdf
 * Implementation is based on https://www.cut-the-knot.org/Curriculum/Combinatorics/JohnsonTrotter.shtml
 */
class Permutation1 {
    @Test fun `permutations of a list`() {
        checkPermutationsFunction { it.permutations().toList() }

        listOf(0, 1).permutations().toList().printed()
        listOf(1, 2, 3, 4).permutations().toList().printed()
        "abcd".toCharArray().toList().permutations().toList().printed()
        0.until(10_000).toList().permutations().take(10).toList()
    }

    companion object {
        private const val left = -1
        private const val right = 1

        private data class Index(val value: Int, var direction: Int) {
            fun reverseDirection() {
                direction = if (direction == left) right else left
            }

            override fun toString() = (if (direction == left) "<" else ">") + value
        }

        fun <E> List<E>.permutations(): Sequence<List<E>> {
            val list = this
            return sequence {
                val indices = list.indices.mapTo(ArrayList()) { Index(it, left) }
                var maxIndex = indices.findLargestMovable()

                yield(list)

                while (maxIndex != null) {
                    val i = indices.indexOf(maxIndex)
                    indices.swap(i, i + maxIndex.direction)
                    yield(indices.map { list[it.value] })

                    indices.forEach { if (it.value > maxIndex!!.value) it.reverseDirection() }
                    maxIndex = indices.findLargestMovable()
                }
            }
        }

        private fun List<Index>.findLargestMovable(): Index? {
            return filterIndexed { i, _ -> canMove(i) }.maxBy { it.value }
        }

        private fun List<Index>.canMove(i: Int): Boolean {
            val j = i + this[i].direction
            return (j >= 0 && j < size) && this[i].value > this[j].value
        }
    }
}
