package katas.kotlin.leetcode.min_moves_to_obtain_string

import datsok.shouldEqual
import org.junit.jupiter.api.Test

/**
 * https://leetcode.com/discuss/interview-question/398026
 *
 * You're given a string S consisting of N letters 'a' and/or 'b'.
 * In one move, you can swap one letter for the other ('a' for 'b' or 'b' for 'a').
 * Write a function that returns the minimum number moves required to obtain a string containing no instances of three identical consecutive letters.
 * N is within the range [0..200_000].
 *
 * Examples
 * 1. S = "baaaaa".
 * Result is 1, because there is one move to "baabaa".
 * 2. S = "baaabbaabbba".
 * Result is 2, there four valid strings, e.g. "bbaabbaabbaa".
 * 3. S = "bbaabab".
 * Result is 0.
 */
class MinMoves {
    @Test fun `it works`() {
        movesCount("") shouldEqual 0
        movesCount("a") shouldEqual 0
        movesCount("b") shouldEqual 0
        movesCount("aaa") shouldEqual 1
        movesCount("baaaaa") shouldEqual 1
        movesCount("baaabbaabbba") shouldEqual 2
        movesCount("bbaabab") shouldEqual 0
    }

    private fun movesCount(s: String): Int {
        var count = 0
        var i = 0
        while (i < s.length) {
            val window = s.substring(i, minOf(i + 3, s.length))
            if (window == "aaa" || window == "bbb") {
                i += 3
                count++
            } else {
                i += 1
            }
        }
        return count
    }
}
