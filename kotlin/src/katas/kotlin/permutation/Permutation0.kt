package katas.kotlin.permutation

import katas.kotlin.shouldHaveSameElementsAs
import org.junit.Test

class Permutation0 {
    // TODO try https://en.wikipedia.org/wiki/Steinhaus%E2%80%93Johnson%E2%80%93Trotter_algorithm

    @Test fun `list permutations`() {
        validatePermutationsFunction({ it.permutations() })
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

fun validatePermutationsFunction(f: (List<Int>) -> List<List<Int>>) {
    f(emptyList()) shouldHaveSameElementsAs listOf(emptyList())

    f(listOf(1)) shouldHaveSameElementsAs listOf(listOf(1))

    f(listOf(1, 2)) shouldHaveSameElementsAs listOf(
        listOf(1, 2), listOf(2, 1)
    )

    f(listOf(1, 2, 3)) shouldHaveSameElementsAs listOf(
        listOf(1, 2, 3), listOf(1, 3, 2),
        listOf(2, 1, 3), listOf(2, 3, 1),
        listOf(3, 1, 2), listOf(3, 2, 1)
    )

    f(listOf(1, 2, 3, 4)) shouldHaveSameElementsAs listOf(
        listOf(1, 2, 3, 4), listOf(1, 2, 4, 3), listOf(1, 3, 2, 4), listOf(1, 3, 4, 2), listOf(1, 4, 2, 3), listOf(1, 4, 3, 2),
        listOf(2, 1, 3, 4), listOf(2, 1, 4, 3), listOf(2, 3, 1, 4), listOf(2, 3, 4, 1), listOf(2, 4, 1, 3), listOf(2, 4, 3, 1),
        listOf(3, 1, 2, 4), listOf(3, 1, 4, 2), listOf(3, 2, 1, 4), listOf(3, 2, 4, 1), listOf(3, 4, 1, 2), listOf(3, 4, 2, 1),
        listOf(4, 1, 2, 3), listOf(4, 1, 3, 2), listOf(4, 2, 1, 3), listOf(4, 2, 3, 1), listOf(4, 3, 1, 2), listOf(4, 3, 2, 1)
    )
}
