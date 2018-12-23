package katas.kotlin.permutation

import kotlincommon.printed
import kotlincommon.swap
import org.junit.Test

class Permutation4 {
    @Test fun `permutations based on swaps`() {
//        mutableListOf(1, 2).permutations().toList().printed()
        mutableListOf(1, 2, 3).permutations().toList().printed()
//        0.until(10_000).toList().permutations().take(10).toList().printed()
        checkPermutationsFunction { it.toMutableList().permutations().toList() }
    }

    companion object {
        private fun <E> MutableList<E>.permutations(n: Int = size - 1): Sequence<List<E>> {
            if (n <= 0) return sequenceOf(ArrayList(this))
            val list = this
            return sequence {
                IntRange(0, n).forEach { i ->
                    list.swap(i, n)
                    yieldAll(list.permutations(n - 1))
                    list.swap(i, n)
                }
            }
        }
    }
}