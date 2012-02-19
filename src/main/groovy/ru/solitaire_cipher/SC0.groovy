package ru.solitaire_cipher

import org.junit.Test
import ru.util.GroovyUtil

/**
 * User: dima
 * Date: 19/02/2012
 */
class SC0 {
  @Test public void aaa() {
    Collection.metaClass.mixin(GroovyUtil)

    assert normalize("Code in Ruby, live longer!") == "CODEINRUBYLIVELONGER"
    assert padToFive("CODEINRUBYLIVELONGER_") == "CODEINRUBYLIVELONGER_XXXX"
    assert toNumbers("CODEINRUBYLIVELONGER") == [3, 15, 4, 5, 9, 14, 18, 21, 2, 25, 12, 9, 22, 5, 12, 15, 14, 7, 5, 18]

    assert asInts("DWJXHYRFDGTMSHPUURXJ") == [4, 23, 10, 24, 8, 25, 18, 6, 4, 7, 20, 13, 19, 8, 16, 21, 21, 18, 24, 10]

    assert sum(asInts("CODEINRUBYLIVELONGER"), asInts("DWJXHYRFDGTMSHPUURXJ")) ==
            [7, 12, 14, 3, 17, 13, 10, 1, 6, 6, 6, 22, 15, 13, 2, 10, 9, 25, 3, 2]
    assert fromInts([7, 12, 14, 3, 17, 13, 10, 1, 6, 6, 6, 22, 15, 13, 2, 10, 9, 25, 3, 2]) ==
            "GLNCQMJAFFFVOMBJIYCB"

    assert encode("CODEINRUBYLIVELONGER", "DWJXHYRFDGTMSHPUURXJ") == "GLNCQMJAFFFVOMBJIYCB"
    assert decode("GLNCQMJAFFFVOMBJIYCB", "DWJXHYRFDGTMSHPUURXJ") == "CODEINRUBYLIVELONGER"

    // TODO
  }

  static decode(String message, String key) {
    fromInts(subtract(asInts(message), asInts(key)))
  }

  static encode(String message, String key) {
    fromInts(sum(asInts(message), asInts(key)))
  }

  static fromInts(def ints) {
    ints.collect { (char) (it + 64) }.join("")
  }

  static subtract(List msgInts, List keyInts) {
    msgInts.injectWithIndex([]) { acc, value, i ->
      def letter = value - keyInts[i % keyInts.size()] + 26
      acc + (letter % 26)
    }
  }

  static sum(List msgInts, List keyInts) {
    msgInts.injectWithIndex([]) { acc, value, i ->
      def letter = value + keyInts[i % keyInts.size()]
      acc + (letter % 26)
    }
  }

  static asInts(String s) {
    toNumbers(padToFive(normalize(s)))
  }

  static toNumbers(String s) {
    s.toCharArray().collect {((int) it) - 64}
  }

  static normalize(String message) {
    message.replaceAll(/[^\w]/, "").toUpperCase()
  }

  static padToFive(String message) {
    message + "X" * ((5 - message.length() % 5) % 5)
  }
}
