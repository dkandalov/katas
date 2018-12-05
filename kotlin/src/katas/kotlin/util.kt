package katas.kotlin

import katas.kotlin.permutation.Permutation1
import kotlin.math.pow

fun <E> MutableList<E>.swap(index1: Int, index2: Int) {
    val tmp = this[index1]
    this[index1] = this[index2]
    this[index2] = tmp
}

fun <E> List<E>.permutations(): Sequence<List<E>> = with(Permutation1) { this@permutations.permutations() }

fun Int.pow(n: Int): Int = this.toDouble().pow(n.toDouble()).toInt()

fun Long.pow(n: Long): Long = this.toDouble().pow(n.toDouble()).toLong()
