package katas.kotlin.leetcode.most_common_word

import datsok.shouldEqual
import org.junit.Test

class MostCommonWordTests {
    @Test fun examples() {
        mostCommonWord(
            paragraph = "Bob hit a ball, the hit BALL flew far after it was hit.",
            banned = arrayOf("hit")
        ) shouldEqual "ball"
        mostCommonWord(
            paragraph = "Bob hit a ball, the hit BALL flew far after it was hit.",
            banned = arrayOf("ball")
        ) shouldEqual "hit"
    }
}

private fun mostCommonWord(paragraph: String, banned: Array<String>): String {
    val words = paragraph.toLowerCase()
        .filter { it.isLetter() || it == ' ' }.split(" ")
        .filterNot { banned.contains(it) }

    val count = HashMap<String, Int>()
    words.forEach { count[it] = count.getOrPut(it, { 0 }) + 1 }

    return count.entries.maxBy { it.value }!!.key
}