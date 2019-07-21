package katas.kotlin.leetcode.palindrome_number

import kotlincommon.test.shouldEqual
import org.junit.Test

class PalindromeNumberTests {
    @Test fun `it works`() {
        0.isPalindrome() shouldEqual true
        1.isPalindrome() shouldEqual true
        10.isPalindrome() shouldEqual false
        100.isPalindrome() shouldEqual false
        (-121).isPalindrome() shouldEqual false
        11.isPalindrome() shouldEqual true

        121.isPalindrome() shouldEqual true
        1221.isPalindrome() shouldEqual true
        12321.isPalindrome() shouldEqual true

        212.isPalindrome() shouldEqual true
        2112.isPalindrome() shouldEqual true
        321123.isPalindrome() shouldEqual true
    }
}

private fun Int.isPalindrome(): Boolean {
    var n = this
    if (n < 0 || (n % 10 == 0 && n != 0)) return false

    var revertedNumber = 0
    while (n > revertedNumber) {
        revertedNumber = revertedNumber * 10 + n % 10
        n /= 10
    }
    return n == revertedNumber || n == revertedNumber / 10
}
