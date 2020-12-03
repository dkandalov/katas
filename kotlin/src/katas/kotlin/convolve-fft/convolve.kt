package katas.kotlin.`convolve-fft`

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.jupiter.api.Test
import kotlin.math.*

class ConvolveTests {
    @Test fun `convolve frequencies of 1,1,2`() {
        //     input: 1 1 2
        //     index: 0 1 2
        // frequency: 0 2 1

        assertThat(
            convolve(listOf(0, 2, 1), listOf(0, 2, 1)),
            //      index: 0  1  2  3  4  5  6  7
            equalTo(listOf(0, 0, 4, 4, 1, 0, 0, 0))
            //                   ^ means that there are four ways to get sum of 2 using two items from the input:
            //                     1+1 (index 1 + index 2), 1+1 (reverse),
            //                     1+1 (index 1 sum with itself), 1+1 (index 2 sum with itself)
        )
    }

    @Test fun `convolve frequencies of -1,0,1,2,-1,-4`() {
        //    input: -1 0 1 2 -1 -4
        //    index: -4-3-2-1 0 1 2 3 4
        // frequency: 1 0 0 2 1 1 1 0 0

        assertThat(
            convolve(listOf(1, 0, 0, 2, 1, 1, 1, 0, 0), listOf(1, 0, 0, 2, 1, 1, 1, 0, 0)),
            //     index: -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7
            equalTo(listOf(1, 0, 0, 4, 2, 2, 6, 4, 5, 6, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
            //                      ^ means that there are four ways to get sum of -5 using two items from the input
        )
    }
}

fun convolve(a: List<Int>, b: List<Int>): List<Int> {
    val resultSize = 0.rangeTo(10)
        .map { 2.0.pow(it).toInt() }
        .find { it > a.size + b.size - 1 }!!

    val aComplex = a.map(Int::toComplex) + List(resultSize - a.size) { Complex(0, 0) }
    val bComplex = b.map(Int::toComplex) + List(resultSize - b.size) { Complex(0, 0) }

    val result = fft(aComplex).zip(fft(bComplex))
        .map { (first, second) -> first * second }

    return rfft(result).map { it.re.roundToInt() }
}

/**
 * Based on rosetta code https://rosettacode.org/wiki/Fast_Fourier_transform#Kotlin
 */
fun fft(a: Array<Complex>) = _fft(a, Complex(0.0, 2.0), 1.0)
fun rfft(a: Array<Complex>) = _fft(a, Complex(0.0, -2.0), 2.0)
fun fft(a: List<Complex>) = _fft(a, Complex(0.0, 2.0), 1.0)
fun rfft(a: List<Complex>) = _fft(a, Complex(0.0, -2.0), 2.0)

private fun _fft(a: List<Complex>, direction: Complex, scalar: Double): List<Complex> =
    _fft(a.toTypedArray(), direction, scalar).toList()

private fun _fft(a: Array<Complex>, direction: Complex, scalar: Double): Array<Complex> =
    if (a.size == 1) {
        a
    } else {
        val n = a.size
        require(n % 2 == 0) { "The Cooley-Tukey FFT algorithm only works when the length of the input is even." }

        var (evens, odds) = Pair(emptyArray<Complex>(), emptyArray<Complex>())
        a.indices.forEach { i ->
            if (i % 2 == 0) evens += a[i]
            else odds += a[i]
        }
        evens = _fft(evens, direction, scalar)
        odds = _fft(odds, direction, scalar)

        val pairs = (0 until n / 2).map {
            val offset = (direction * (Math.PI * it / n)).exp * odds[it] / scalar
            val base = evens[it] / scalar
            Pair(base + offset, base - offset)
        }
        var (left, right) = Pair(emptyArray<Complex>(), emptyArray<Complex>())
        pairs.forEach { (l, r) ->
            left += l
            right += r
        }
        left + right
    }

class Complex(val re: Double, val im: Double) {
    private val a = "%1.3f".format(re)
    private val b = "%1.3f".format(abs(im))
    val exp: Complex by lazy { Complex(cos(im), sin(im)) * (cosh(re) + sinh(re)) }

    constructor(re: Int, im: Int) : this(re.toDouble(), im.toDouble())

    infix operator fun plus(x: Complex) = Complex(re + x.re, im + x.im)
    infix operator fun minus(x: Complex) = Complex(re - x.re, im - x.im)
    infix operator fun times(x: Double) = Complex(re * x, im * x)
    infix operator fun times(x: Complex) = Complex(re * x.re - im * x.im, re * x.im + im * x.re)
    infix operator fun div(x: Double) = Complex(re / x, im / x)

    fun magnitude(): Double = sqrt(re.pow(2) + im.pow(2))

    override fun toString() = when {
        b == "0.000" -> a
        a == "0.000" -> b + 'i'
        im > 0       -> a + " + " + b + 'i'
        else         -> a + " - " + b + 'i'
    }
}

fun Int.toComplex() = Complex(this.toDouble(), 0.0)