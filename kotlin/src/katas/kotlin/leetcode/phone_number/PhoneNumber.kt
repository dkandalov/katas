package katas.kotlin.leetcode.phone_number

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/letter-combinations-of-a-phone-number/
 */
class PhoneNumberTests {
    @Test fun `find all possible letter combinations that a number could represent`() {
        "".letterCombinations() shouldEqual emptyList()
        "1".letterCombinations() shouldEqual emptyList()
        "2".letterCombinations() shouldEqual listOf("a", "b", "c")
        "3".letterCombinations() shouldEqual listOf("d", "e", "f")
        "23".letterCombinations() shouldEqual listOf("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf")
    }
}

private fun String.letterCombinations(): List<String> {
    if (isEmpty()) return emptyList()
    val lettersByNumber = mapOf(
        '1' to emptyList(),
        '2' to listOf("a", "b", "c"),
        '3' to listOf("d", "e", "f")
    )
    val letters = lettersByNumber[first()] ?: error("")
    val combinations = drop(1).letterCombinations()
    return if (combinations.isEmpty()) letters
    else letters.flatMap { letter -> combinations.map { letter + it } }
}
