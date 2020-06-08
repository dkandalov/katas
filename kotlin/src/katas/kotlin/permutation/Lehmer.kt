package katas.kotlin.permutation

import datsok.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.ArrayList

class LehmerTests {
    @Test fun `map permutation to Lehmer code`() {
        emptyList<Int>().toLehmerCode() shouldEqual LehmerCode()
        listOf(0).toLehmerCode() shouldEqual LehmerCode(0)

        listOf(0, 1, 2).toLehmerCode() shouldEqual LehmerCode(0, 0, 0)
        listOf(0, 2, 1).toLehmerCode() shouldEqual LehmerCode(0, 1, 0)
        listOf(1, 0, 2).toLehmerCode() shouldEqual LehmerCode(1, 0, 0)
        listOf(1, 2, 0).toLehmerCode() shouldEqual LehmerCode(1, 1, 0)
        listOf(2, 0, 1).toLehmerCode() shouldEqual LehmerCode(2, 0, 0)
        listOf(2, 1, 0).toLehmerCode() shouldEqual LehmerCode(2, 1, 0)

        listOf(1, 0, 4, 3, 2).toLehmerCode() shouldEqual LehmerCode(1, 0, 2, 1, 0)
    }

    @Test fun `map Lehmer code to a number`() {
        LehmerCode().toLong() shouldEqual 0
        LehmerCode(123).toLong() shouldEqual 0

        LehmerCode(0, 0, 0).toLong() shouldEqual 0
        LehmerCode(0, 1, 0).toLong() shouldEqual 1
        LehmerCode(1, 0, 0).toLong() shouldEqual 2
        LehmerCode(1, 1, 0).toLong() shouldEqual 3
        LehmerCode(2, 0, 0).toLong() shouldEqual 4
        LehmerCode(2, 1, 0).toLong() shouldEqual 5

        LehmerCode(0, 0, 0, 0, 1).toLong() shouldEqual 0
        LehmerCode(0, 0, 0, 1, 0).toLong() shouldEqual 1
        LehmerCode(0, 0, 1, 0, 0).toLong() shouldEqual 2
        LehmerCode(0, 1, 0, 0, 0).toLong() shouldEqual 6
        LehmerCode(1, 0, 0, 0, 0).toLong() shouldEqual 24

        LehmerCode(1, 0, 2, 1, 0).toLong() shouldEqual 29
    }

    @Test fun `map number to a Lehmer code`() {
        0.toLehmerCode() shouldEqual LehmerCode(0)
        0.toLehmerCode(size = 1) shouldEqual LehmerCode(0)
        0.toLehmerCode(size = 2) shouldEqual LehmerCode(0, 0)

        0.toLehmerCode() shouldEqual LehmerCode(0)
        1.toLehmerCode() shouldEqual LehmerCode(1, 0)
        2.toLehmerCode() shouldEqual LehmerCode(1, 0, 0)
        3.toLehmerCode() shouldEqual LehmerCode(1, 1, 0)
        4.toLehmerCode() shouldEqual LehmerCode(2, 0, 0)
        5.toLehmerCode() shouldEqual LehmerCode(2, 1, 0)

        0.toLehmerCode() shouldEqual LehmerCode(0)
        1.toLehmerCode() shouldEqual LehmerCode(1, 0)
        2.toLehmerCode() shouldEqual LehmerCode(1, 0, 0)
        6.toLehmerCode() shouldEqual LehmerCode(1, 0, 0, 0)
        24.toLehmerCode() shouldEqual LehmerCode(1, 0, 0, 0, 0)

        29.toLehmerCode() shouldEqual LehmerCode(1, 0, 2, 1, 0)
    }

    @Test fun `map Lehmer code to a permutation`() {
        LehmerCode().toPermutation() shouldEqual emptyList()
        LehmerCode(0).toPermutation() shouldEqual listOf(0)

        LehmerCode(0, 0, 0).toPermutation() shouldEqual listOf(0, 1, 2)
        LehmerCode(0, 1, 0).toPermutation() shouldEqual listOf(0, 2, 1)
        LehmerCode(1, 0, 0).toPermutation() shouldEqual listOf(1, 0, 2)
        LehmerCode(1, 1, 0).toPermutation() shouldEqual listOf(1, 2, 0)
        LehmerCode(2, 0, 0).toPermutation() shouldEqual listOf(2, 0, 1)
        LehmerCode(2, 1, 0).toPermutation() shouldEqual listOf(2, 1, 0)

        LehmerCode(1, 0, 2, 1, 0).toPermutation() shouldEqual listOf(1, 0, 4, 3, 2)
        LehmerCode(1, 0, 2, 1, 0).toPermutation(listOf(1, 2, 3, 4, 5)) shouldEqual listOf(2, 1, 5, 4, 3)
        LehmerCode(1, 0, 2, 1, 0).toPermutation(listOf('a', 'b', 'c', 'd', 'e')) shouldEqual listOf('b', 'a', 'e', 'd', 'c')
    }
}


data class LehmerCode(val value: List<Int>) {
    constructor(vararg value: Int): this(value.toList())

    fun toLong(): Long {
        var factorial = 1
        var result = 0L
        value.dropLast(1).asReversed().forEachIndexed { index, n ->
            factorial *= index + 1
            result += n * factorial
        }
        return result
    }

    fun toPermutation(): List<Int> {
        val indices = value.indices.toMutableList()
        return value.map {
            indices.removeAt(it)
        }
    }

    fun <T> toPermutation(list: List<T>): List<T> {
        return toPermutation().map { list[it] }
    }
}

fun List<Int>.toLehmerCode(): LehmerCode {
    val bitSet = BitSet(size)
    return LehmerCode(map {
        bitSet[it] = true
        it - bitSet.get(0, it).cardinality()
    })
}

fun Int.toLehmerCode(size: Int = -1): LehmerCode = toLong().toLehmerCode(size)

fun Long.toLehmerCode(size: Int = -1): LehmerCode {
    val result = ArrayList<Int>()
    result.add(0)

    var value = this
    var factorial = 1
    var iteration = 1

    while (value != 0L) {
        factorial *= iteration
        iteration += 1

        val divisor = value / factorial
        val remainder = divisor % iteration
        result.add(0, remainder.toInt())

        value -= remainder * factorial
    }

    if (size != -1) {
        IntRange(result.size, size - 1).forEach { result.add(0, 0) }
    }
    return LehmerCode(result)
}