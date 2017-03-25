package katas.kotlin

import io.kotlintest.specs.StringSpec

fun <T> T.printed(): T {
    println(this)
    return this
}

fun Collection<*>.join(): String = joinToString("")

fun <E> Iterable<E>.skip(n: Int): Iterable<E> {
    val iterator = iterator()
    0.until(n).forEach {
        if (!iterator.hasNext()) throw IllegalStateException()
        iterator.next()
    }
    return object : Iterable<E> {
        override fun iterator() = iterator
    }
}

fun <E> List<E>.sliding(windowSize: Int): List<List<E>> {
    return (0..(size - windowSize)).map { subList(it, it + windowSize) }
}

class UtilFunctionsTest : StringSpec() {
    init {
        "sliding window" {
            listOf<Int>().sliding(2) shouldEqual listOf<List<Int>>()
            listOf(1).sliding(2) shouldEqual listOf<List<Int>>()
            listOf(1, 2).sliding(2) shouldEqual listOf(listOf(1, 2))
            listOf(1, 2, 3).sliding(2) shouldEqual listOf(listOf(1, 2), listOf(2, 3))
            listOf(1, 2, 3, 4).sliding(2) shouldEqual listOf(listOf(1, 2), listOf(2, 3), listOf(3, 4))
            listOf(1, 2, 3, 4).sliding(3) shouldEqual listOf(listOf(1, 2, 3), listOf(2, 3, 4))
        }

        "skipping elements" {
            listOf(1, 2, 3).skip(0).toList() shouldEqual listOf(1, 2, 3)
            listOf(1, 2, 3).skip(1).toList() shouldEqual listOf(2, 3)
            listOf(1, 2, 3).skip(2).toList() shouldEqual listOf(3)
            listOf(1, 2, 3).skip(3).toList() shouldEqual listOf<Int>()
        }
    }
}