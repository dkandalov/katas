package ru.katas.n19

import org.junit.Test

import static org.hamcrest.CoreMatchers.equalTo
import static org.junit.Assert.assertThat

/**
 * User: dima
 * Date: 08/08/2012
 */
@SuppressWarnings("GroovyMissingReturnStatement")
class WordChain10 {

  private def shortestWordChain = new ShortestWordChain()

  @Test void shouldFindSimpleWordChains() {
    shortestWordChain.with {
      assert it.find("aaa", "bbb", []) == []
      assert it.find("aaa", "aaa", ["aaa"]) == ["aaa"]
      assert it.find("aaa", "bbb", ["aaa", "aab", "abb", "bbb"]) == ["aaa", "aab", "abb", "bbb"]
    }
  }

  @Test void shouldFindShortestWordChain() {
    def dict = ["aaa", "aab", "abb", "abc", "acc", "bbb"]
    assert shortestWordChain.find("aaa", "bbb", dict) == ["aaa", "aab", "abb", "bbb"]
  }

  @Test(timeout = 10000L)
  void shouldFindShortestWordChain_FromCatToDog() {
    assertThat(shortestWordChain.find("cat", "dog", loadDict()), equalTo(["cat", "cag", "cog", "dog"]))
  }

  static class ShortestWordChain {

    def Map<String, Collection<String>> moves

    def find(String fromWord, String toWord, Collection dict) {
      moves = findAllValidMoves(fromWord, dict.findAll{ it.length() == toWord.length() })
      sortMovesByNextMovesAmount(moves)
      def newDict = new HashSet(moves.keySet())

      for (i in (1..newDict.size() + 1)) {
        def chain = doFind(fromWord, toWord, newDict - fromWord, 1, i)
        if (!chain.empty) return chain
      }
      []
    }

    private def doFind(String fromWord, String toWord, Collection dict, int depth, int minDepth) {
      if (depth >= minDepth) return []
      if (fromWord == toWord) {
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

    private static def sortMovesByNextMovesAmount(Map<String, Collection<String>> moves) {
      moves.each { entry ->
        moves[entry.key] = moves[entry.key].sort { moves[it].size() }
      }
    }
  }

  @Test void shouldFindAllValidMoves() {
    assert ShortestWordChain.findAllValidMoves("aaa", ["aaa", "aab", "aba", "ccc"]) == [
            "aaa" : ["aab", "aba"],
            "aab" : ["aaa"],
            "aba" : ["aaa"],
    ]
  }

  private static List<String> loadDict() {
    new File("/usr/share/dict/words").readLines().collect() {it.toLowerCase()}
  }

  @Test void shouldLoadRealDictionary() {
    assert loadDict().size() > 10000
  }

  @Test void shouldDetermineIfMoveIsValid() {
    shortestWordChain.with {
      assert !canMove("a", "a")
      assert canMove("a", "b")

      assert canMove("aaa", "aab")
      assert canMove("aaa", "aba")
      assert canMove("aaa", "baa")
      assert !canMove("aaa", "abb")
    }
  }
}