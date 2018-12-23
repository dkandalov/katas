package katas.kotlin.permutation

import katas.kotlin.sort.mergesort.printed
import org.junit.Test

class Permutation0 {
    @Test fun `list permutations`() {
        listOf(1, 2, 3).permutations().toList().printed()
        listOf(1, 2, 3, 4).permutations().toList().printed()
        checkPermutationsFunction { it.permutations().toList() }
        checkPermutationsFunction { it.permutationsSequence().toList() }
    }

    companion object {
        fun List<Int>.permutations(): List<List<Int>> {
            if (size <= 1) return listOf(this)
            return flatMap { item ->
                (this - item).permutations().map { permutation ->
                    permutation + item
                }
            }
        }

        fun List<Int>.permutationsSequence(): Sequence<List<Int>> {
            if (size <= 1) return sequenceOf(this)
            val list = this
            return sequence {
                list.forEach { item ->
                    yieldAll((list - item).permutations().map { permutation ->
                        permutation + item
                    })
                }
            }
        }
    }
}
