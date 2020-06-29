package katas.kotlin.leetcode.add_and_search_word

class AddAndSearchWord2 {
    private class Node(
        val value: Char,
        val nodes: MutableMap<Char, Node> = mutableMapOf(),
        var isEndOfTheWord: Boolean = false
    ) {
        fun add(word: String) {
            if (word.isEmpty()) {
                isEndOfTheWord = true
                return
            }
            val child: Node = this.nodes.getOrPut(word.first()) { Node(word.first()) }
            child.add(word.drop(1))
        }

        fun search(word: String): Boolean {
            if (word.isEmpty()) return isEndOfTheWord
            if (word.first() == '.') {
                return nodes.values.any { it.search(word.drop(1)) }
            } else {
                val childNode = this.nodes[word.first()] ?: return false
                return childNode.search(word.drop(1))
            }
        }
    }

    private val root = Node('0')

    fun addWord(word: String) {
        root.add(word.toLowerCase())
    }

    fun search(word: String): Boolean {
        return root.search(word)
    }
}