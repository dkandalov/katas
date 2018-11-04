package katas.kotlin.hackerrank

import kotlincommon.printed
import katas.kotlin.shouldEqual
import org.junit.Test
import java.util.*

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    main(generateSequence { scanner.nextLine() })
}

private fun main(input: Sequence<String>, output: (Any?) -> Unit = { println(it) }) {
    val store = StringsStore()

    val i = input.iterator()
    val amountOfStrings = i.next().toInt()
    0.until(amountOfStrings).forEach {
        store.add(s = i.next())
    }

    val amountOfQueries = i.next().toInt()
    0.until(amountOfQueries).forEach {
        output(store.numberOfMatches(s = i.next()))
    }
}


class StringsStore {
    private var strings = ArrayList<String>()

    fun add(s: String) {
        strings.add(s)
    }

    fun numberOfMatches(s: String): Int {
        return strings.count { it == s }
    }
}

class StringsStoreTests {
    @Test fun `basic example`() {
        val input = """
            |4
            |aba
            |baba
            |aba
            |xzxb
            |3
            |aba
            |xzxb
            |ab
        """.toLineSequence()
        val output = OutputRecorder()

        main(input, output)

        output.text shouldEqual """
            |2
            |1
            |0
        """.trimMargin() + "\n"

    }
}

fun String.toLineSequence(): Sequence<String> = trim().trimMargin().split("\n").asSequence()

class OutputRecorder: (Any?) -> Unit {
    var text = ""
    override fun invoke(o: Any?) {
        text += o.toString() + "\n"
    }
}