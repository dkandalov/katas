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
        dictionary.addWord("bar")
        dictionary.search("foo") shouldEqual true
        dictionary.search("fo.") shouldEqual true
        dictionary.search("fo..") shouldEqual true
    }
}

private class WordDictionary {
    private data class Node(val char: Char, val children: MutableMap<Char, Node> = HashMap())

    private val root = Node('.')

    fun addWord(word: String) {
        var node = root
        word.toCharArray().forEach { char ->
            node = node.children.getOrPut(char, { Node(char) })
        }
    }

    /**
     * A word can contain the dot character '.' to represent any single letter.
     */
    fun search(word: String): Boolean {
        var node = root
        word.toCharArray().forEach { char ->
            if (char != '.') {
                node = node.children[char] ?: return false
            }
        }
        return true
    }
}