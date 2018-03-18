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
        validatePermutationsFunction({ it.permutations().toList() })

        listOf(1, 2).permutations().toList().printed()
        listOf(1, 2, 3).permutations().toList().printed()
        "abcd".toCharArray().toList().permutations().toList().printed()
        0.until(10_000).toList().permutations().take(10).toList().printed()
    }

    private val left = -1
    private val right = 1

    private data class DirectedInt(val value: Int, var direction: Int)

    private fun <E> List<E>.permutations(): Sequence<List<E>> {
        val list = this
        val values = list.indices.map { DirectedInt(it, left) }.toMutableList()
        var permutationsCount = list.size.factorial()

        return buildSequence {
            yield(values.map { list[it.value] })
            permutationsCount -= ONE

            while (permutationsCount > ZERO) {
                val maxItem = values.filterIndexed { index, _ -> values.isMobile(index) }.maxBy { it.value }!!
                val i = values.indexOf(maxItem)

                values.swap(i, i + maxItem.direction)
                yield(values.map { list[it.value] })

                values.filter { it.value > maxItem.value }
                    .forEach { it.direction = if (it.direction == left) right else left }
                permutationsCount -= ONE
            }
        }
    }

    private fun List<DirectedInt>.isMobile(i: Int): Boolean {
        val j = i + this[i].direction
        if (j < 0 || j >= size) return false
        return this[i].value > this[j].value
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
