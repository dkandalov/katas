package katas.kotlin.leetcode.longest_string_of_vowels

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/233724/Amazon-online-assessment-Longest-string-made-up-of-only-vowels
 */
class LongestStringOfVowelsTests {
    @Test fun examples() {
        longestString("earthproblem") shouldEqual "eao"
        longestString("letsgosomewhere") shouldEqual "ee"

        longestString("XXXeeeXXX") shouldEqual "eee"
        longestString("aaaXXXeeeXXXooo") shouldEqual "aaaeeeooo"
        longestString("aaaXXiXeeeXiiXXooo") shouldEqual "aaaeeeooo"
    }
}

private fun longestString(s: String): String {
    val s1 = s.takeWhile { it.isVowel() }
    val s3 = s.takeLastWhile { it.isVowel() }
    val s2 = s.drop(s1.length).dropLast(s3.length).let { s2 ->
        var from = 0
        var max = ""
        s2.forEachIndexed { i, char ->
            if (!char.isVowel()) {
                if (i - from > max.length) max = s2.substring(from, i)
                from = i + 1
            }
        }
        max
    }
    return s1 + s2 + s3
}

@Suppress("unused")
private fun longestString__(s: String): String {
    var max = 0
    var result = ""
    s.length.allRanges().forEach { r1 ->
        val s2 = s.removeRange(r1)
        s2.length.allRanges().forEach { r2 ->
            val s3 = s2.removeRange(r2)
            if (s3.all { it.isVowel() } && s3.length > max) {
                max = s3.length
                result = s3
            }
        }
    }
    return result
}

private fun Int.allRanges() = sequence {
    (0 until this@allRanges).forEach { i ->
        (i until this@allRanges).forEach { j ->
            yield(IntRange(i, j))
        }
    }
}

private fun Char.isVowel(): Boolean = setOf('a', 'e', 'i', 'o', 'u').contains(this)
