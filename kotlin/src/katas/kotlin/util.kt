package katas.kotlin

fun <E> MutableList<E>.swap(i1: Int, i2: Int) {
    val tmp = this[i1]
    this[i1] = this[i2]
    this[i2] = tmp
}
