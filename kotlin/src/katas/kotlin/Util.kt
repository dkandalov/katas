package katas.kotlin

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import io.kotlintest.specs.StringSpec

infix fun <T> T.shouldEqual(that: T) {
    assertThat(this, equalTo(that))
}

infix fun <T> T.shouldNotEqual(that: T) {
    assertThat(this, !equalTo(that))
}

fun <T> T.printed(f: (T) -> String = { it.toString() }): T {
    println(f(this))
    return this
}

fun <T> Iterable<T>.join(separator: CharSequence = ", ", prefix: CharSequence = "", postfix: CharSequence = "",
                         limit: Int = -1, truncated: CharSequence = "...", transform: ((T) -> CharSequence)? = null): String {
    return this.joinToString(separator, prefix, postfix, limit, truncated, transform)
}

fun <E> Iterable<E>.skip(n: Int): Iterable<E> {
    val iterator = iterator()
    0.until(n).forEach {
        if (!iterator.hasNext()) throw IllegalStateException()
        iterator.next()
    }
    return object: Iterable<E> {
        override fun iterator() = iterator
    }
}

fun <E> List<E>.sliding(windowSize: Int): List<List<E>> {
    return (0..(size - windowSize)).map { subList(it, it + windowSize) }
}

fun <E> List<E>.tail() = drop(1)

fun <E> List<E>.permutations(): List<List<E>> =
    if (size <= 1) listOf(this)
    else flatMap { item ->
        (this - item).permutations().map { it: List<E> -> listOf(item) + it }
    }


class UtilFunctionsTest: StringSpec() {
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

        "permutation of elements" {
            listOf<Int>().permutations() shouldEqual listOf(listOf<Int>())
            listOf(1).permutations() shouldEqual listOf(listOf(1))
            listOf(1, 2).permutations() shouldEqual listOf(listOf(1, 2), listOf(2, 1))
            listOf(1, 2, 3).permutations() shouldEqual listOf(
                listOf(1, 2, 3),
                listOf(1, 3, 2),
                listOf(2, 1, 3),
                listOf(2, 3, 1),
                listOf(3, 1, 2),
                listOf(3, 2, 1)
            )
        }
    }
}