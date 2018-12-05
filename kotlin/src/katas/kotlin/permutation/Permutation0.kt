package katas.kotlin.permutation

import org.junit.Test

class Permutation0 {
    @Test fun `list permutations`() {
        checkPermutationsFunction { it.permutations() }
    }

    private fun List<Int>.permutations(): List<List<Int>> {
        if (size <= 1) return listOf(this)
        return flatMap { item ->
            (this - item).permutations().map { permutation ->
                permutation + item
            }
        }
    }
}
