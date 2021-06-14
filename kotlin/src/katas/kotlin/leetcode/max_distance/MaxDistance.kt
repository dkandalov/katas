package katas.kotlin.leetcode.max_distance

import nonstdlib.tail
import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/350363/Google-or-OA-2018-or-Max-Distance
 */
class MaxDistanceTests {
    @Test fun examples() {
        maxDistance("1011000", "1011110") shouldEqual Pair("1011000", "1011110")
        maxDistance("1011000", "1011110", "1011----") shouldEqual Pair("1011110", "1011----")
        maxDistance("1011000--", "1011110", "1011----") shouldEqual Pair("1011----", "1011000--")
    }
}

data class TrieNode(val value: Char, val children: HashMap<Char, TrieNode> = HashMap()) {
    fun add(s: String) {
        if (s.isEmpty()) return
        val child = children.getOrPut(s.first()) { TrieNode(s.first()) }
        child.add(s.tail())
    }

    fun maxDepthChildren(): Pair<String, String> {
        val (child1, child2) = children.values.map { it.maxDepthChild() }.sortedBy { it.first }.takeLast(2)
        return Pair(child1.second, child2.second)
    }

    private fun maxDepthChild(): Pair<Int, String> {
        val max = children.values.map { it.maxDepthChild() }.maxByOrNull { it.first } ?: Pair(-1, "")
        return Pair(max.first + 1, value.toString() + max.second)
    }
}

private fun maxDistance(vararg strings: String): Pair<String, String> {
    var trie = TrieNode(0.toChar())
    strings.forEach { trie.add(it) }

    var prefix = ""
    while (trie.children.size == 1) {
        trie = trie.children.values.first()
        prefix += trie.value
    }

    val (child1, child2) = trie.maxDepthChildren()
    return Pair(prefix + child1, prefix + child2)
}
