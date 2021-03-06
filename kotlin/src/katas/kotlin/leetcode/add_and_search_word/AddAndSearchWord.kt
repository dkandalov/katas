package katas.kotlin.leetcode.add_and_search_word

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/add-and-search-word-data-structure-design/
 */
class AddAndSearchWordTests {
    @Test fun `add and search words`() {
        val dictionary = WordDictionary()
        dictionary.addWord("foo")
        dictionary.addWord("foz")
        dictionary.addWord("bar")

        dictionary.search("f") shouldEqual false
        dictionary.search("fo") shouldEqual false
        dictionary.search("foo") shouldEqual true
        dictionary.search("fooo") shouldEqual false

        dictionary.search("fo.") shouldEqual true
        dictionary.search("f.o") shouldEqual true
        dictionary.search(".oo") shouldEqual true
        dictionary.search(".o.") shouldEqual true
        dictionary.search("...") shouldEqual true
        dictionary.search("....") shouldEqual false
        dictionary.search("fo..") shouldEqual false
    }
}

private class WordDictionary {
    private data class Node(
        val children: MutableMap<Char, Node> = HashMap()
    )

    private val root = Node()

    fun addWord(word: String) {
        root.add(word)
    }

    /**
     * A pattern can contain the dot character '.' to represent any single letter.
     */
    fun search(pattern: String): Boolean = root.search(pattern)

    companion object {
        private val sentinel = Node()

        private fun Node.add(word: String) {
            var node = this
            word.forEach { char ->
                node = node.children.getOrPut(char, defaultValue = { Node() })
            }
            node.children[word.last()] = sentinel
        }

        private fun Node.search(pattern: String): Boolean {
            var node = this
            pattern.forEachIndexed { i, char ->
                when (char) {
                    '.'  -> return node.children.values.any { it.search(pattern.substring(i + 1)) }
                    else -> node = node.children[char] ?: return false
                }
            }
            return node.children.containsValue(sentinel)
        }
    }
}