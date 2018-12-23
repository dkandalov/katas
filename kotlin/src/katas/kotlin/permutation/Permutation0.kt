package katas.kotlin.permutation

import katas.kotlin.sort.mergesort.printed
import kotlincommon.test.shouldEqual
import org.junit.Test

class Permutation0 {

    @Test fun `permutations by removal`() {
        listOf(1, 2, 3).permutations_remove() shouldEqual listOf(
            listOf(3, 2, 1),
            listOf(2, 3, 1),
            listOf(3, 1, 2),
            listOf(1, 3, 2),
            listOf(2, 1, 3),
            listOf(1, 2, 3)
        )
        listOf(1, 2, 3, 4).permutations_remove().printed()

        checkPermutationsFunction { it.permutations_remove().toList() }
        checkPermutationsFunction { it.permutationsSequence().toList() }
    }

    @Test fun `permutations by addition`() {
        mutableListOf(1, 2, 3).permutations_add() shouldEqual listOf(
            listOf(1, 2, 3),
            listOf(2, 1, 3),
            listOf(2, 3, 1),
            listOf(1, 3, 2),
            listOf(3, 1, 2),
            listOf(3, 2, 1)
        )
        mutableListOf(1, 2, 3, 4).permutations_add().toList().printed()
        checkPermutationsFunction { it.toMutableList().permutations_add().toList() }
    }

    @Test fun `permutations by addition minimum swaps`() {
        mutableListOf(1, 2, 3).permutations_add_min_change() shouldEqual listOf(
            listOf(1, 2, 3),
            listOf(2, 1, 3),
            listOf(2, 3, 1),
            listOf(3, 2, 1),
            listOf(3, 1, 2),
            listOf(1, 3, 2)
        )
        mutableListOf(1, 2, 3, 4).permutations_add_min_change().toList().printed()
        checkPermutationsFunction { it.toMutableList().permutations_add_min_change().toList() }
    }

    companion object {
        private fun List<Int>.permutations_remove(): List<List<Int>> =
            if (size <= 1) listOf(this)
            else flatMap { item ->
                (this - item).permutations_remove().map { permutation ->
                    permutation + item
                }
            }

        private fun List<Int>.permutationsSequence(): Sequence<List<Int>> {
            if (size <= 1) return sequenceOf(this)
            val list = this
            return sequence {
                list.forEach { item ->
                    yieldAll((list - item).permutationsSequence().map { permutation ->
                        permutation + item
                    })
                }
            }
        }

        private fun MutableList<Int>.permutations_add(): List<List<Int>> {
            if (size <= 1) return listOf(this)
            val item = first()
            val subPermutations = drop(1).toMutableList().permutations_add()
            return subPermutations.flatMap { subPermutation ->
                IntRange(0, subPermutation.size).map { i ->
                    ArrayList(subPermutation).apply { add(i, item) }
                }
            }
        }

        private fun MutableList<Int>.permutations_add_min_change(): List<List<Int>> {
            if (size <= 1) return listOf(this)
            val item = first()
            val subPermutations = drop(1).toMutableList().permutations_add_min_change()
            return subPermutations.mapIndexed { index, subPermutation ->
                val permutation = IntRange(0, subPermutation.size).map { i ->
                    ArrayList(subPermutation).apply { add(i, item) }
                }
                if (index % 2 != 0) permutation.reversed() else permutation
            }.flatten()
        }
    }
}
