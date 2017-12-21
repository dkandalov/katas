package katas.kotlin.hackerrank

import katas.kotlin.shouldEqual
import org.junit.Test
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.OutputStream
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
}

fun String.toInputStream(): InputStream = ByteArrayInputStream(this.toByteArray())

class ArrayManipulation(size: Int) {
    private val array = Array(size, { 0L })

    fun update(from: Int, to: Int, value: Int) {
        from.until(to).forEach {
            array[it] = array[it] + value
        }
    }

    fun max(): Long {
        return array.max()!!
    }
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
        array.update(from - 1, to - 1, value)
    }
    output.writer().use {
        it.write(array.max().toString())
    }
}

//fun main(args: Array<String>) {
//    main(System.`in`, System.out)
//}
