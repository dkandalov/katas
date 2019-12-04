package katas.kotlin.permutation

import kotlincommon.*
import kotlincommon.test.*
import org.junit.*
import java.util.*

class AllPermutationTypes {

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
        checkPermutationsFunction { it.permutations_remove_sequence().toList() }
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

        mutableListOf(1, 2, 3, 4).permutations_add_min_change().printed()
        mutableListOf(0, 1, 2, 3).permutations_add_min_change().printed()
            .map { it.toLehmerCode().toLong().printed() }

        checkPermutationsFunction { it.toMutableList().permutations_add_min_change().toList() }
    }

    @Test fun `lexicographic permutations`() {
        listOf(1, 2, 3).permutations_lexicographic() shouldEqual listOf(
            listOf(1, 2, 3),
            listOf(1, 3, 2),
            listOf(2, 1, 3),
            listOf(2, 3, 1),
            listOf(3, 1, 2),
            listOf(3, 2, 1)
        )
        listOf(1, 2, 3, 4).permutations_lexicographic().join("\n").printed()

        listOf(0, 1, 2, 3).permutations_lexicographic()
            .map { it.toLehmerCode().toLong() } shouldEqual LongRange(0, 23).toList()

        checkPermutationsFunction { it.toMutableList().permutations_lexicographic().toList() }
    }

    @Test fun `lexicographic permutations using Lehmer code`() {
        listOf(1, 2, 3).permutations_lehmer() shouldEqual listOf(
            listOf(1, 2, 3),
            listOf(1, 3, 2),
            listOf(2, 1, 3),
            listOf(2, 3, 1),
            listOf(3, 1, 2),
            listOf(3, 2, 1)
        )
        listOf(1, 2, 3, 4).permutations_lehmer().join("\n").printed()

        checkPermutationsFunction { it.permutations_lehmer().toList() }
    }

    @Test fun `Heap's permutations`() {
        mutableListOf(1, 2, 3).permutations_heaps() shouldEqual listOf(
            listOf(1, 2, 3),
            listOf(2, 1, 3),
            listOf(3, 1, 2),
            listOf(1, 3, 2),
            listOf(2, 3, 1),
            listOf(3, 2, 1)
        )
        mutableListOf(1, 2, 3, 4).permutations_heaps().join("\n").printed()
        checkPermutationsFunction { it.toMutableList().permutations_heaps().toList() }
    }


    companion object {
        private fun List<Int>.permutations_remove(): List<List<Int>> =
            if (size <= 1) listOf(this)
            else flatMap { item ->
                (this - item)
                    .permutations_remove()
                    .map { permutation -> permutation + item }
            }

        private fun List<Int>.permutations_remove_sequence(): Sequence<List<Int>> {
            if (size <= 1) return sequenceOf(this)
            val list = this
            return sequence {
                list.forEach { item ->
                    val permutations = (list - item)
                        .permutations_remove_sequence()
                        .map { permutation -> permutation + item }
                    yieldAll(permutations)
                }
            }
        }

        private fun MutableList<Int>.permutations_add(): List<List<Int>> {
            if (size <= 1) return listOf(this)
            val firstItem = first()
            val subPermutations = drop(1).toMutableList().permutations_add()
            return subPermutations.flatMap { subPermutation ->
                (0..subPermutation.size)
                    .map { i -> ArrayList(subPermutation).apply { add(i, firstItem) } }
            }
        }

        private fun MutableList<Int>.permutations_add_min_change(): List<List<Int>> {
            if (size <= 1) return listOf(this)
            val item = first()
            val subPermutations = drop(1).toMutableList().permutations_add_min_change()
            return subPermutations.mapIndexed { index, subPermutation ->
                val permutation = (0..subPermutation.size)
                    .map { i -> ArrayList(subPermutation).apply { add(i, item) } }
                if (index % 2 != 0) permutation.reversed() else permutation
            }.flatten()
        }

        /**
         * https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
         * https://www.nayuki.io/page/next-lexicographical-permutation-algorithm
         */
        private fun List<Int>.permutations_lexicographic(): List<List<Int>> {
            val values = this
            val list = indices.toMutableList()
            val result = ArrayList<List<Int>>()
            result.add(list.map { values[it] })

            while (true) {
                val i = list.indices.windowed(2).findLast { (i1, i2) -> list[i1] < list[i2] }?.first() ?: return result
                val j = IntRange(i + 1, list.size - 1).findLast { list[i] < list[it] }!!
                list.swap(i, j)
                list.subList(i + 1, list.size).reverse()
                result.add(list.map { values[it] })
            }
        }

        private fun List<Int>.permutations_lehmer(): List<List<Int>> {
            if (isEmpty()) return listOf(emptyList())

            return LongRange(0, size.longFactorial() - 1)
                .map { it.toLehmerCode(size = size).toPermutation(this) }
        }

        private fun MutableList<Int>.permutations_heaps(n: Int = size): List<List<Int>> {
            if (n <= 1) return listOf(ArrayList(this))
            return sequence {
                (1..n).forEach { i ->
                    yieldAll(permutations_heaps(n - 1))
                    if (n % 2 == 0) swap(i - 1, n - 1)
                    else swap(0, n - 1)
                }
            }.toList()
        }
    }
}
