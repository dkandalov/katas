package katas.kotlin.palindromes

import katas.kotlin.shouldEqual
import org.junit.Test
import kotlin.coroutines.experimental.buildSequence

class Palindromes {

    @Test fun `palindrome example from the task`() {
        findLongestPalindromes("sqrrqabccbatudefggfedvwhijkllkjihxymnnmzpop") shouldEqual listOf(
            Palindrome("hijkllkjih", IntRange(23, 32)),
            Palindrome("defggfed", IntRange(13, 20)),
            Palindrome("abccba", IntRange(5, 10))
        )
    }

    @Test fun `find all substrings of a string`() {
        "abcd".allSubstrings().map { it.first }.toList() shouldEqual listOf(
            "abcd", "abc", "ab", "a",
            "bcd", "bc", "b",
            "cd", "c",
            "d"
        )
    }

    @Test fun `determine if string is a palindrome`() {
        "".isPalindrome() shouldEqual true
        "a".isPalindrome() shouldEqual true
        "aa".isPalindrome() shouldEqual true

        "ab".isPalindrome() shouldEqual false
        "ba".isPalindrome() shouldEqual false

        "aba".isPalindrome() shouldEqual true
        "abba".isPalindrome() shouldEqual true
        "aabba".isPalindrome() shouldEqual false
    }

    data class Palindrome(val value: String, val range: IntRange) {
        override fun toString() = "Text: $value, Index: ${range.first}, Length: ${value.length}"
    }

    fun findLongestPalindromes(s: String, maxAmount: Int = 3, minPalindromeSize: Int = 2): List<Palindrome> {
        val result = ArrayList<Palindrome>()
        s.allSubstrings()
            .filter { (substring, _) ->
                substring.length >= minPalindromeSize && substring.isPalindrome()
            }
            .forEach { (substring, range) ->
                if (result.none { it.range.contains(range) }) {
                    result.add(Palindrome(substring, range))
                }
            }
        return result.sortedBy { -it.value.length }.distinct().take(maxAmount)
    }

    fun String.allSubstrings(): Sequence<Pair<String, IntRange>> = buildSequence {
        val s = this@allSubstrings
        0.until(s.length).forEach { from ->
            s.length.downTo(from + 1).forEach { to ->
                val range = IntRange(from, to - 1)
                val substring = s.substring(range)
                yield(Pair(substring, range))
            }
        }
    }

    tailrec fun String.isPalindrome(): Boolean =
        if (length < 2) true
        else if (first() != last()) false
        else substring(1, length - 1).isPalindrome()

    private fun IntRange.contains(range: IntRange) = contains(range.first) && contains(range.last)
}
