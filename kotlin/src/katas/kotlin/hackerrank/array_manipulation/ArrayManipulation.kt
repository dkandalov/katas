package katas.kotlin.hackerrank.array_manipulation

import kotlincommon.test.shouldEqual
import org.junit.Test
import java.io.*
import java.util.*

/**
 * https://www.hackerrank.com/challenges/crush
 */
fun main() {
    main(System.`in`, System.out)
}

fun main(input: InputStream, output: OutputStream) {
    val scanner = Scanner(input).useDelimiter("[\n\\s]+")
    val n = scanner.nextInt()
    val array = ArrayManipulation(size = n)

    val m = scanner.nextInt()
    0.until(m).forEach {
        val from = scanner.nextInt()
        val to = scanner.nextInt()
        val value = scanner.nextInt()
        array.update(from - 1, to, value)
    }
    output.writer().use {
        it.write(array.max().toString())
    }
}

class ArrayManipulation(size: Int) {
    private val array = LongArray(size)

    fun update(from: Int, to: Int, value: Int) {
        if (from > 0) array[from - 1] = array[from - 1] - value
        if (to > 0) array[to - 1] = array[to - 1] + value
    }

    fun max(): Long {
        var max = Long.MIN_VALUE
        var i = array.size - 1
        var sum = 0L
        while (i >= 0) {
            sum += array[i]
            array[i] = sum
            if (sum > max) max = sum
            i--
        }
        return max
    }
}

class ArrayManipulationTests {
    @Test fun example() {
        val input = """
            |5 3
            |1 2 100
            |2 5 100
            |3 4 100
            """.trimMargin().toInputStream()
        val output = ByteArrayOutputStream()

        main(input, output)

        output.toString() shouldEqual "200"
    }

    @Test fun `test case 4`() {
        val input = File("src/katas/kotlin/hackerrank/array_manipulation/ArrayManipulation-testcase-4.txt").inputStream()
        val output = ByteArrayOutputStream()

        main(input, output)

        output.toString() shouldEqual "7542539201"
    }

    @Test fun `test case 7`() {
        val input = File("src/katas/kotlin/hackerrank/array_manipulation/ArrayManipulation-testcase-7.txt").inputStream()
        val output = ByteArrayOutputStream()

        main(input, output)

        output.toString() shouldEqual "2497169732"
    }
}

fun String.toInputStream(): InputStream = ByteArrayInputStream(this.toByteArray())
