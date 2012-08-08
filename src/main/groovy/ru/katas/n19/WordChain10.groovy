package ru.katas.n19

import org.junit.Test

import static org.hamcrest.CoreMatchers.equalTo
import static org.junit.Assert.assertThat

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

  @Test(timeout = 70000L)
  void shouldFindShortestWordChain_FromCatToDog() {
    assertThat(findShortestChain("cat", "dog", loadDict()), equalTo(["cat", "cag", "cog", "dog"]))
  }

  def findShortestChain(String fromWord, String toWord, Collection dict) {
    moves = findAllValidMoves(fromWord, dict.findAll{ it.length() == toWord.length() })
    sortMovesByNextMovesAmount(moves)
    def newDict = new HashSet(moves.keySet())
    doFind(fromWord, toWord, newDict - fromWord, 1, newDict.size() + 1)
  }

  private static def sortMovesByNextMovesAmount(Map<String, Collection<String>> moves) {
    moves.each { entry ->
      moves[entry.key] = moves[entry.key].sort { moves[it].size() }
    }
  }

  private static Map<String, Collection<String>> findAllValidMoves(String fromWord, Collection dict, Map result = [:]) {
    Set set = new TreeSet([fromWord])
    while (!set.empty) {
      fromWord = set.pollFirst()
      def nextWords = dict.findAll{ String word -> canMove(fromWord, word) }.unique()
      result[fromWord] = nextWords

      set.addAll(nextWords.findAll{ !result.containsKey(it) })
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
    if (fromWord == toWord) {
      println depth
      return [toWord]
    }

    def result = []
    def nextWords = moves[fromWord].intersect(dict)
    dict = new HashSet(dict)
    dict.removeAll(nextWords)
    dict.add(toWord)

    for (String word in nextWords) {
      def chain = doFind(word, toWord, dict, depth + 1, minDepth)
      if (!chain.empty) {
        minDepth = depth + chain.size()
        chain.add(0, fromWord)
        result = chain
      }
    }
    result
  }

  private static boolean canMove(String fromWord, String toWord) {
    if (fromWord.length() != toWord.length()) return false

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