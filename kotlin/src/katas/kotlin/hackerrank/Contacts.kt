package katas.kotlin.hackerrank

import katas.kotlin.shouldEqual
import org.junit.Ignore
import org.junit.Test
import java.io.File
import java.util.*

class ContactsTests {

    @Test fun `add and find contacts`() {
        val contacts = TrieNode()
        contacts.add("value")
        contacts.add("value2")
        contacts.amountOfMatches("val") shouldEqual 2
        contacts.amountOfMatches("value") shouldEqual 2
        contacts.amountOfMatches("value2") shouldEqual 1
        contacts.amountOfMatches("value3") shouldEqual 0
    }

    @Test fun `hackerrank test`() {
        val contacts = TrieNode()
        contacts.add("hack")
        contacts.add("hackerrank")
        contacts.amountOfMatches("hac") shouldEqual 2
        contacts.amountOfMatches("hak") shouldEqual 0
    }

    @Ignore
    @Test fun `adding maximum amount of elements`() {
        val contacts = TrieNode()
        val random = Random(123)
        0.until(100000).forEach { _ ->
            contacts.add(random.nextString(21))
        }
    }

    @Ignore
    @Test fun `hackerrank test case 2 (fixing performance)`() {
        val lines = File("src/katas/kotlin/hackerrank/Contacts-testcase-2-input.txt").readLines()
        main(lines.asSequence())
    }

    @Ignore
    @Test fun `hackerrank test case 3 (fixing performance)`() {
        val lines = File("src/katas/kotlin/hackerrank/Contacts-testcase-3-input.txt").readLines()
        main(lines.asSequence())
    }

    @Ignore
    @Test fun `hackerrank test case 5 (fixing performance)`() {
        val lines = File("src/katas/kotlin/hackerrank/Contacts-testcase-5-input.txt").readLines()
        main(lines.asSequence())
    }

    @Ignore
    @Test fun `hackerrank test case 12 (fixing performance)`() {
        val lines = File("src/katas/kotlin/hackerrank/Contacts-testcase-12-input.txt").readLines()
        main(lines.asSequence())
    }

    private fun Random.nextString(length: Int): String {
        fun safeChar(): Char {
            val surrogateStart = 0xD800
            val res = nextInt(surrogateStart - 1) + 1
            return res.toChar()
        }
        return List(length, { safeChar() }).joinToString("")
    }
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    main(lines = generateSequence { scanner.nextLine() })
}

private fun main(lines: Sequence<String>) {
    val i = lines.iterator()
    val n = i.next().toInt()

//    var addDuration: Duration = Duration.ZERO
//    var findDuration: Duration = Duration.ZERO

    val tree = TrieNode()
    0.until(n).forEach {
        val parts = i.next().split(Regex(" +"))
        val command = parts[0]
        val value = parts[1]
        if (command == "add") {
//            addDuration += measureTimeMillis {
                tree.add(value)
//            }
        } else if (command == "find") {
//            findDuration += measureTimeMillis {
                println(tree.amountOfMatches(value))
//            }
        }
    }

//    println("addDuration = $addDuration")
//    println("findDuration = $findDuration")
}


private data class TrieNode(
    val children: MutableMap<Char, TrieNode> = HashMap(),
    var leafCount: Int = 0
) {
    fun add(s: String) {
        var node = this
        s.forEach { c ->
            node.children.putIfAbsent(c, TrieNode())
            node = node.children[c]!!
            node.leafCount++
        }
    }

    fun amountOfMatches(s: String): Int {
        var node = this
        for (i in s.indices) {
            val child = node.children[s[i]]
            if (child == null) return 0 else node = child
        }
        return node.leafCount
    }

    override fun toString() = children.toString()
}
