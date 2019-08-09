package katas.kotlin.leetcode.add_and_search_word

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/add-and-search-word-data-structure-design/
 */
class AddAndSearchWordTests {
    @Test fun `add and search words`() {
        val dictionary = WordDictionary()
        dictionary.addWord("foo")
        dictionary.search("foo") shouldEqual true
    }
}

private class WordDictionary {
    fun addWord(word: String) {
    }

    /**
     * Returns if the word is in the data structure.
     * A word could contain the dot character '.' to represent any one letter.
     */
    fun search(word: String): Boolean {
        return true
    }
}