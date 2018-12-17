package katas.kotlin

import katas.kotlin.permutation.Permutation1

fun <T> Collection<T>.doesNotContain(t: T): Boolean = !this.contains(t)

fun <T> List<T>.tail(): List<T> = drop(1)

fun <E> List<E>.permutations(): Sequence<List<E>> = with(Permutation1) { this@permutations.permutations() }

