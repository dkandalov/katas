package katas.kotlin.permutation

import org.junit.Test

class Permutation2 {
    @Test fun `permutation of a list`() {
        checkPermutationsFunction(::permutationsOf)
    }

    private fun <E> permutationsOf(list: List<E>): List<List<E>> =
        if (list.size <= 1) listOf(list)
        else list.flatMap { item ->
            permutationsOf(list.filterNot { it == item }).map { it + item }
        }
}