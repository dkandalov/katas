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
        dictionary.search("fooo") shouldEqual false

        dictionary.search("fo.") shouldEqual true
        dictionary.search("f.o") shouldEqual true
        dictionary.search(".oo") shouldEqual true
        dictionary.search("...") shouldEqual true
        dictionary.search("....") shouldEqual false
        dictionary.search("fo..") shouldEqual false
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
    fun search(word: String, startNode: Node = root): Boolean {
        var node = startNode
        word.toCharArray().forEachIndexed { i, char ->
            if (char == '.') {
                return node.children.values.any { search(word.substring(i + 1), it) }
            } else {
                node = node.children[char] ?: return false
            }
        }
        return true
    }
}