package katas.kotlin.`bloom-filter`

import datsok.shouldEqual
import nonstdlib.printedAs
import org.junit.Test
import java.io.File
import java.security.MessageDigest
import java.util.*

class BloomFilter0 {
    private class Filter(private val bitSet: BitSet = BitSet()) {
        fun add(word: String): Boolean {
            val alreadyAdded = contains(word)
            return if (alreadyAdded) true
            else {
                word.toIndices().forEach { bitSet[it] = true }
                false
            }
        }

        fun contains(word: String): Boolean =
            word.toIndices().all { bitSet[it] }

        private fun String.toIndices(): List<Int> {
            return sha256().slide(3).map {
                0.or(it[0] + 128)
                 .or((it[1] + 128).shl(8))
                 .or((it[2] + 128).shl(16))
            }
        }

        private fun String.sha256(): ByteArray {
            val messageDigest = MessageDigest.getInstance("SHA-256")
            messageDigest.update(this.toByteArray(Charsets.UTF_8))
            return messageDigest.digest()!!
        }
    }

    @Test fun `adding one word`() {
        val filter = Filter()

        filter.add("cat") shouldEqual false
        filter.add("cat") shouldEqual true
        filter.contains("cat") shouldEqual true
    }

    @Test fun `adding two words`() {
        val filter = Filter()

        filter.add("cat") shouldEqual false
        filter.contains("cat") shouldEqual true

        filter.add("dog") shouldEqual false
        filter.contains("dog") shouldEqual true
    }

    @Test fun `adding words from dictionary`() {
        val filter = Filter()

        val words = File("src/katas/kotlin/words.txt").readLines().printedAs { "word amount: ${it.size}" }
        var clashCount = 0
        words.forEach {
            val alreadyAdded = filter.add(it)
            if (alreadyAdded) clashCount++
        }

        clashCount.printedAs { "clash count: $it" } shouldEqual 1
    }

    @Test fun `slide byte array`() {
        byteArrayOf(1).slide(1) shouldEqual listOf(listOf<Byte>(1))
        byteArrayOf(1).slide(2) shouldEqual listOf()

        byteArrayOf(1, 2).slide(1) shouldEqual listOf(listOf<Byte>(1), listOf<Byte>(2))
        byteArrayOf(1, 2).slide(2) shouldEqual listOf(listOf<Byte>(1, 2))
        byteArrayOf(1, 2, 3, 4).slide(2) shouldEqual listOf(listOf<Byte>(1, 2), listOf<Byte>(3, 4))
    }
}

private fun ByteArray.slide(size: Int): List<List<Byte>> {
    val result = ArrayList<List<Byte>>()
    var list = ArrayList<Byte>()
    this.forEach { byte ->
        if (list.size == size) {
            result += list
            list = ArrayList()
        }
        list.add(byte)
    }
    if (list.size == size) result += list
    return result
}
