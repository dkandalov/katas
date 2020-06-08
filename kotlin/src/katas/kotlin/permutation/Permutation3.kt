package katas.kotlin.permutation

import nonstdlib.tail
import org.junit.Test

class Permutation3 {
    @Test fun `permutation of a list`() {
        checkPermutationsFunction(::permutationsOf)
    }

    private fun <E> permutationsOf(list: List<E>): List<List<E>> =
        if (list.size <= 1) listOf(list)
        else permutationsOf(list.tail()).flatMap { permutation ->
            0.rangeTo(permutation.size).map { insertIndex ->
                permutation.toMutableList().apply { add(insertIndex, list.first()) }
            }
        }
}