package katas.kotlin.leetcode.compare_strings

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/352458/Google-or-OA-Fall-Internship-2019-or-Compare-Strings
 */
class CompareStringsTests {
    @Test fun `count smaller strings examples`() {
        countSmallerStrings("abcd,aabc,bd", "aaa,aa") shouldEqual listOf(3, 2)
        countSmallerStrings("abcd,abcd,aabc,bd", "aaa,aa") shouldEqual listOf(4, 3)
        countSmallerStrings("abcd,abcd,abcd,aabc,bd", "aaa,aa") shouldEqual listOf(5, 4)
        countSmallerStrings("abcd,abcd,abcd,aabc,aabc,bd", "aaa,aa") shouldEqual listOf(6, 5)
    }

    @Test fun `string comparison examples`() {
        "".frequencyCompareTo("") shouldEqual 0
        "".frequencyCompareTo("a") shouldEqual -1
        "a".frequencyCompareTo("") shouldEqual 1

        "aaa".frequencyCompareTo("aaa") shouldEqual 0

        "abcd".frequencyCompareTo("aaa") shouldEqual -1
        "aaa".frequencyCompareTo("abcd") shouldEqual 1

        "a".frequencyCompareTo("bb") shouldEqual -1
        "bb".frequencyCompareTo("a") shouldEqual 1

        "aabc".frequencyCompareTo("aa") shouldEqual 0
    }
}

private fun countSmallerStrings(a: String, b: String): List<Int> {
    val aFrequencies = a.split(",").map { it.minCharFrequency() }.sorted()
    val bFrequencies = b.split(",").map { it.minCharFrequency() }
    return bFrequencies.map { bFreq ->
        var index = aFrequencies.binarySearch(bFreq)
        index = if (index < 0) -(index + 1) else index
        while (index + 1 < aFrequencies.size && aFrequencies[index] == aFrequencies[index + 1]) index++
        index
    }
}

private fun String.frequencyCompareTo(that: String): Int =
    this.minCharFrequency().compareTo(that.minCharFrequency())

private fun String.minCharFrequency(): Int {
    val min = minOrNull()
    return count { it == min }
}
