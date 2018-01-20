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

fun <E> List<E>.sliding(windowSize: Int): List<List<E>> {
    return (0..(size - windowSize)).map { subList(it, it + windowSize) }
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
    }
}