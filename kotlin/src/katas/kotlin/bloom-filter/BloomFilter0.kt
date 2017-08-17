package katas.kotlin.`bloom-filter`

import katas.kotlin.printed
import katas.kotlin.shouldEqual
import org.junit.Test
import java.security.MessageDigest
import java.util.*

class BloomFilter0 {
    private class Filter(private val bitSet: BitSet = BitSet(256)) {
        fun add(word: String): Boolean {
            val alreadyAdded = contains(word)
            return if (alreadyAdded) true
            else {
                word.sha256().forEach {
                    bitSet[it.toUnsignedInt()] = true
                }
                false
            }
        }

        fun contains(word: String): Boolean =
            word.sha256().all { bitSet[it.toUnsignedInt()] }

        private fun Byte.toUnsignedInt() = toInt() + 128

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

    private fun ByteArray.toBinaryString() = this
        .map {
            String.format("%8s", Integer.toBinaryString(it.toInt().and(0xFF))).replace(' ', '0')
        }
        .joinToString("")

    private fun BitSet.toBinaryString() =
        0.until(size())
            .map { if (get(it)) "1" else "0" }
            .joinToString("")
}