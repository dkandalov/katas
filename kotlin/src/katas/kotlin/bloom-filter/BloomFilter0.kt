package katas.kotlin.`bloom-filter`

import katas.kotlin.printed
import org.junit.Test
import java.security.MessageDigest
import java.util.*

class BloomFilter0 {
    @Test fun `aaa`() {
        val bitSet = BitSet(1)
        bitSet.set(2, true)
        bitSet.set(1, true)
        bitSet.toBinaryString().printed()

        val messageDigest = MessageDigest.getInstance("SHA-256")
        val text = "word"
        messageDigest.update(text.toByteArray(Charsets.UTF_8))
        messageDigest.digest().apply{
            toList().printed()
            toBinaryString().printed()
        }
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