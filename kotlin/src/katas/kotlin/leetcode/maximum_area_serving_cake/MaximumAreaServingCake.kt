package katas.kotlin.leetcode.maximum_area_serving_cake

import kotlincommon.pow
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Test
import kotlin.math.PI
import kotlin.math.abs

class MaximumAreaServingCakeTests {
    @Test fun examples() {
        findLargestPiece(radii = arrayOf(1, 1, 1, 2, 2, 3), numberOfGuests = 6).roundTo(digits = 4) shouldEqual "7.0686"
        findLargestPiece(radii = arrayOf(4, 3, 3), numberOfGuests = 3).roundTo(4) shouldEqual "28.2743"
        findLargestPiece(radii = arrayOf(6, 7), numberOfGuests = 12).roundTo(4) shouldEqual "21.9911"
    }
}

private fun findLargestPiece(radii: Array<Int>, numberOfGuests: Int): Double {
    return findLargestPiece(radii.map { it * it * PI }, numberOfGuests)
}

fun findLargestPiece(cakes: List<Double>, numberOfGuests: Int): Double {
    fun feed(piece: Double): Double {
        val cc = ArrayList(cakes)
        var i = 0
        var guests = numberOfGuests
        while (i < cc.size && guests > 0) {
            if (cc[i] > piece) {
                cc[i] -= piece
                guests--
            } else {
                i++
            }
        }
        return if (guests > 0) -1.0 else cc.sum()
    }

    var min = 0.0
    var max = cakes.sum() / numberOfGuests

    while (min < max && !min.closeTo(max)) {
        val mid = (max + min) / 2
        val f = feed(mid)
        if (f > 0 && f.closeTo(0.0)) {
            return mid
        } else if (f > 0) {
            min = mid
        } else if (f < 0) {
            max = mid
        }
    }
    return min
}

private fun Double.closeTo(that: Double) = abs(this - that) <= 0.000001

private fun Double.roundTo(digits: Int): String = String.format("%.${digits}f", this)
