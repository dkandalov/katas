package katas.kotlin.hackerrank

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
    fun add(s: String) {
        TODO("not implemented")
    }

    fun numberOfMatches(s: String): Int {
        TODO("not implemented")
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
        """.trim().trimMargin()
        println(input)
    }
}