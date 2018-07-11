@file:Suppress("EXPERIMENTAL_FEATURE_WARNING")

package katas.kotlin.permutation

import kotlincommon.printed
import org.junit.Test
import java.math.BigInteger
import java.math.BigInteger.ONE
import java.math.BigInteger.ZERO
import kotlin.coroutines.experimental.buildSequence

/**
 * The original paper http://www.ams.org/journals/mcom/1963-17-083/S0025-5718-1963-0159764-2/S0025-5718-1963-0159764-2.pdf
 * Implementation is based on https://www.cut-the-knot.org/Curriculum/Combinatorics/JohnsonTrotter.shtml
 */
class Permutation1 {
    @Test fun `permutations of a list`() {
        validatePermutationsFunction { it.permutations().toList() }

        listOf(1, 2).permutations().toList().printed()
        listOf(1, 2, 3).permutations().toList().printed()
        "abcd".toCharArray().toList().permutations().toList().printed()
        0.until(10_000).toList().permutations().take(10).toList().printed()
    }

    private val left = -1
    private val right = 1

    private data class Index(val value: Int, var direction: Int)

    private fun <E> List<E>.permutations(): Sequence<List<E>> {
        val list = this
        return buildSequence {
            var count = list.size.factorial()
            val indices = list.indices.mapTo(ArrayList()) { Index(it, left) }

            yield(indices.map { list[it.value] })
            count -= ONE

            while (count > ZERO) {
                val maxIndex = indices.filterIndexed { index, _ -> indices.isMobile(index) }.maxBy { it.value }!!
                val i = indices.indexOf(maxIndex)
                indices.swap(i, i + maxIndex.direction)

                yield(indices.map { list[it.value] })
                count -= ONE

                indices.filter { it.value > maxIndex.value }
                    .forEach { it.direction = if (it.direction == left) right else left }
            }
        }
    }

    private fun List<Index>.isMobile(i: Int): Boolean {
        val j = i + this[i].direction
        return if (j < 0 || j >= size) false
        else this[i].value > this[j].value
    }
}

private fun Int.factorial(): BigInteger = toBigInteger().factorial()

private fun BigInteger.factorial(): BigInteger {
    var result = ONE
    var n = this
    while (n > ONE) {
        result *= n
        n -= ONE
    }
    return result
}

private fun <E> MutableList<E>.swap(i1: Int, i2: Int) {
    val tmp = this[i1]
    this[i1] = this[i2]
    this[i2] = tmp
}
