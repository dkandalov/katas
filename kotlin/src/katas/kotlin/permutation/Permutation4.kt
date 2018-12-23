package katas.kotlin.permutation

import kotlincommon.printed
import kotlincommon.swap
import org.junit.Test

class Permutation4 {
    @Test fun `permutations based on swaps`() {
//        mutableListOf(1, 2).permutationsSwap().toList().printed()
        mutableListOf(1, 2, 3).permutationsSwap().toList().printed()
        mutableListOf(1, 2, 3, 4).permutationsSwap().toList().printed()
//        0.until(10_000).toList().permutationsSwap().take(10).toList().printed()
        checkPermutationsFunction { it.toMutableList().permutationsSwap().toList() }
    }

    companion object {
        private fun <E> MutableList<E>.permutationsSwap(n: Int = size - 1): Sequence<List<E>> {
            if (n <= 0) return sequenceOf(ArrayList(this))
            val list = this
            return sequence {
                IntRange(0, n).forEach { i ->
                    list.swap(i, n)
                    yieldAll(list.permutationsSwap(n - 1))
                    list.swap(i, n)
                }
            }
        }
    }
}