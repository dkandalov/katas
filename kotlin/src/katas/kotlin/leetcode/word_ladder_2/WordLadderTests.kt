package katas.kotlin.leetcode.word_ladder_2

import kotlincommon.test.*
import org.junit.*

/**
 * https://leetcode.com/problems/word-ladder-ii/
 *
 * Given two words (beginWord and endWord), and a dictionary's word list,
 * find all shortest transformation sequence(s) from beginWord to endWord, such that:
 *  - Only one letter can be changed at a time
 *  - Each transformed word must exist in the word list. Note that beginWord is not a transformed word.
 *
 * Note:
 *  - Return an empty list if there is no such transformation sequence.
 *  - All words have the same length.
 *  - All words contain only lowercase alphabetic characters.
 *  - You may assume no duplicates in the word list.
 *  - You may assume beginWord and endWord are non-empty and are not the same.
 */
class WordLadderTests {
    @Test fun `some examples`() {
        findLadders(beginWord = "hit", endWord = "cog", wordList = emptyList()) shouldEqual emptySet()

        findLadders(
            beginWord = "hit",
            endWord = "cog",
            wordList = listOf("hot", "dot", "dog", "lot", "log")
        ) shouldEqual emptySet()

//        findLadders(
//            beginWord = "hit",
//            endWord = "cog",
//            wordList = listOf("hot", "dot", "dog", "lot", "log", "cog")
//        ) shouldEqual setOf(
//            listOf("hit", "hot", "dot", "dog", "cog"),
//            listOf("hit", "hot", "lot", "log", "cog")
//        )
    }
}

private typealias Solution = List<String>

private fun findLadders(beginWord: String, endWord: String, wordList: List<String>): Set<Solution> {
    if (beginWord == endWord) return setOf(listOf(endWord))

    val nextWords = wordList.filter { it.diff(beginWord) == 1 }
    return nextWords
        .flatMap { findLadders(it, endWord, wordList - it) }
        .mapTo(HashSet()) { listOf(beginWord) + it }
}

private fun String.diff(that: String) =
    zip(that).sumBy { if (it.first != it.second) 1 else 0 }
