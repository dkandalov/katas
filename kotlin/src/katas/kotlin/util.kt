package katas.kotlin

import katas.kotlin.permutation.Permutation1

fun <E> List<E>.permutations(): Sequence<List<E>> = with(Permutation1) { this@permutations.permutations() }

fun <T> T.printed(prefix: String = "", f: (T) -> String = { it.toString() }): T {
    println(prefix + f(this))
    return this
}

