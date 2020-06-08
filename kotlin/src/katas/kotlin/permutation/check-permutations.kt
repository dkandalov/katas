package katas.kotlin.permutation

import katas.kotlin.shouldHaveSameElementsAs
import datsok.shouldEqual

fun checkPermutationsFunction(permutationsOf: (List<Int>) -> List<List<Int>>) {
    permutationsOf(emptyList()) shouldEqual listOf(emptyList())
    permutationsOf(listOf(1)) shouldEqual listOf(listOf(1))

    permutationsOf(listOf(1, 2)) shouldHaveSameElementsAs listOf(
        listOf(1, 2), listOf(2, 1)
    )

    permutationsOf(listOf(1, 2, 3)) shouldHaveSameElementsAs listOf(
        listOf(1, 2, 3), listOf(1, 3, 2),
        listOf(2, 1, 3), listOf(2, 3, 1),
        listOf(3, 1, 2), listOf(3, 2, 1)
    )

    permutationsOf(listOf(1, 2, 3, 4)) shouldHaveSameElementsAs listOf(
        listOf(1, 2, 3, 4), listOf(1, 2, 4, 3), listOf(1, 3, 2, 4), listOf(1, 3, 4, 2), listOf(1, 4, 2, 3), listOf(1, 4, 3, 2),
        listOf(2, 1, 3, 4), listOf(2, 1, 4, 3), listOf(2, 3, 1, 4), listOf(2, 3, 4, 1), listOf(2, 4, 1, 3), listOf(2, 4, 3, 1),
        listOf(3, 1, 2, 4), listOf(3, 1, 4, 2), listOf(3, 2, 1, 4), listOf(3, 2, 4, 1), listOf(3, 4, 1, 2), listOf(3, 4, 2, 1),
        listOf(4, 1, 2, 3), listOf(4, 1, 3, 2), listOf(4, 2, 1, 3), listOf(4, 2, 3, 1), listOf(4, 3, 1, 2), listOf(4, 3, 2, 1)
    )
}