package katas.kotlin.leetcode.phone_number

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/letter-combinations-of-a-phone-number/
 */
class PhoneNumberTests {
    @Test fun `find all possible letter combinations that a number could represent`() {
        "".letterCombinations() shouldEqual listOf()
        "1".letterCombinations() shouldEqual listOf()
    }
}

private fun String.letterCombinations(): List<String> {
    return emptyList()
}
