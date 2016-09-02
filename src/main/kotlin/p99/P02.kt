package p99

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

fun <T> penultimate(list: List<T>) = list[list.lastIndex - 1]

class P02Test {
    @Test fun `get penultimate element`() {
        assertThat(penultimate(listOf(1, 1, 2, 3, 5, 8)), equalTo(5))
    }

    @Test(expected = IndexOutOfBoundsException::class)
    fun `penultimate element in list smaller than 2`() {
        penultimate(listOf<Int>(1))
    }
}
