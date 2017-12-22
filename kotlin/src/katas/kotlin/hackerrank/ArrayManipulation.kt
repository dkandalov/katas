package katas.kotlin.hackerrank

import katas.kotlin.shouldEqual
import org.junit.Test
import java.io.*
import java.util.*

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
        val input = File("src/katas/kotlin/hackerrank/ArrayManipulation-testcase-4.txt").inputStream()
        val output = ByteArrayOutputStream()

        main(input, output)

        output.toString() shouldEqual "7542539201"
    }

    @Test fun `test case 7`() {
        val input = File("src/katas/kotlin/hackerrank/ArrayManipulation-testcase-7.txt").inputStream()
        val output = ByteArrayOutputStream()

        main(input, output)

        output.toString() shouldEqual "2497169732"
    }
}

fun String.toInputStream(): InputStream = ByteArrayInputStream(this.toByteArray())

class ArrayManipulation(size: Int) {
    private val array = Array(size, { 0L })
    private val updates = ArrayList<Update>()

    fun update(from: Int, to: Int, value: Int) {
        updates.add(Update(from - 1, -value))
        updates.add(Update(to - 1, value))
    }

    fun max(): Long {
        updates.sortBy { -it.i }

        var i = array.size - 1
        var ui = 0
        var sum = 0L
        while (i >= 0) {
            while (ui < updates.size && updates[ui].i >= i) {
                sum += updates[ui].value
                ui++
            }
            array[i] = sum
            i--
        }

        return array.max()!!
    }

    data class Update(val i: Int, val value: Int)
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

//fun main(args: Array<String>) {
//    main(System.`in`, System.out)
//}
