package ru.katas.n19

import org.junit.Test

/**
 * User: dima
 * Date: 08/08/2012
 */
class WordChain10 {

  @Test void shouldFindSimpleWordChains() {
    assert findShortestChain("aaa", "bbb", []) == []
    assert findShortestChain("aaa", "aaa", ["aaa"]) == ["aaa"]
    assert findShortestChain("aaa", "bbb", ["aaa", "aab", "abb", "bbb"]) == ["aaa", "aab", "abb", "bbb"]
  }

  @Test void shouldFindShortestWordChain() {
    def dict = ["aaa", "aab", "abb", "abc", "acc", "bbb"]
    assert findShortestChain("aaa", "bbb", dict) == ["aaa", "aab", "abb", "bbb"]
  }

  @Test(timeout = 10000L)
  void shouldFindShortestWordChain_FromCatToDog() {
    assert findShortestChain("cat", "dog", loadDict()) == ["cat", "...", "dog"]
  }

  def findShortestChain(String fromWord, String toWord, Collection dict) {
    moves = findAllValidMoves(fromWord, dict)
    doFind(fromWord, toWord, dict - fromWord, 1, dict.size() + 1)
  }

  private static Map<String, Collection<String>> findAllValidMoves(String fromWord, Collection dict, Map result = [:]) {
    if (result.containsKey(fromWord)) return

    def nextWords = dict.findAll{ String word -> canMove(fromWord, word) }
    result[fromWord] = nextWords
    nextWords.each { String word ->
      findAllValidMoves(word, dict, result)
    }
    result
  }

  @Test void shouldFindAllValidMoves() {
    assert findAllValidMoves("aaa", ["aaa", "aab", "aba", "ccc"]) == [
            "aaa" : ["aab", "aba"],
            "aab" : ["aaa"],
            "aba" : ["aaa"],
    ]
  }

  def Map<String, Collection<String>> moves

  private def doFind(String fromWord, String toWord, Collection dict, int depth, int minDepth) {
    if (depth >= minDepth) return []
    if (fromWord == toWord) return [toWord]

    def result = []
    for (String word in moves[fromWord]) {
      def chain = doFind(word, toWord, dict - word, depth + 1, minDepth)
      if (!chain.empty) {
        result = [fromWord] + chain
        minDepth = depth + chain.size()
      }
    }
    result
  }

  private static boolean canMove(String fromWord, String toWord) {
    int diffs = 0
    def list = toWord.toList()
    for (c in fromWord) {
      if (!list.remove(c)) {
        diffs++
        if (diffs > 1) return false
      }
    }
    diffs == 1
  }

  private static List<String> loadDict() {
    new File("/usr/share/dict/words").readLines().collect() {it.toLowerCase()}
  }

  @Test void shouldLoadRealDictionary() {
    assert loadDict().size() > 10000
  }

  @Test void shouldDetermineIfMoveIsValid() {
    assert !canMove("a", "a")
    assert canMove("a", "b")

    assert canMove("aaa", "aab")
    assert canMove("aaa", "aba")
    assert canMove("aaa", "baa")
    assert !canMove("aaa", "abb")
  }
}