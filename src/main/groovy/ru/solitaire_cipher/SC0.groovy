package ru.solitaire_cipher

import org.junit.Test
import ru.util.GroovyUtil
import ru.util.Pomodoro

/**
 * User: dima
 * Date: 19/02/2012
 */
@Pomodoro("3.5")
class SC0 {
  @Test public void decodingExamples() {
    assert encode(("a".."z").join(""), "DWJXHYRFDGTMSHPUURXJ") == "EYMBMEYNMQEYFVEKLJQDYSGVGYPDBE"
    assert decode("EYMBMEYNMQEYFVEKLJQDYSGVGYPDBE", "DWJXHYRFDGTMSHPUURXJ") == ("A".."Z").join("") + "XXXX"

    assert decode("CLEPK HHNIY CFPWH FDFEH", "DWJXHYRFDGTMSHPUURXJ") == "YOURCIPHERISWORKINGX"
    assert decode("ABVAW LWZSY OORYK DUPVH", "DWJXHYRFDGTMSHPUURXJ") == "WELCOMETORUBYQUIZXXX"
  }

  @Test public void keyGeneration() {
    assert createDeck() == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
            21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
            41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, "A", "B"]

    assert moveCard("A", 1, ["A", 1, 2, 3]) == [1, "A", 2, 3]
    assert moveCard("A", 2, ["A", 1, 2, 3]) == [1, 2, "A", 3]
    assert moveCard("A", 2, [1, 2, "A", 3, 4]) == [1, 2, 3, 4, "A"]
    assert moveCard("A", 2, [0, 1, 2, "A", 3]) == [0, "A", 1, 2, 3]
    assert moveCard("A", 1, createDeck()) == (1..52) + ["B", "A"]
    assert moveCard("B", 2, (1..52) + ["B", "A"]) == [1, "B"] + (2..52) + ["A"]

    assert tripleCut([1, 2, "A", 3, "B", 4, 5]) == [4, 5, "A", 3, "B", 1, 2]
    assert tripleCut([1, "B"] + (2..52) + ["A"]) == ["B"] + (2..52) + ["A", 1]

    assert countCut([2, 3, 4, 5, 1]) == [3, 4, 5, 2, 1]
    assert countCut([1, 3, 4, 5, 2]) == [4, 5, 1, 3, 2]
    assert countCut(["B"] + (2..52) + ["A", 1]) == (2..52) + ["A", "B", 1]

    assert takeLetter([2, 3, 4, 5]) == "D"
    assert takeLetter([3, 5, 4, 1]) == "A"
    assert takeLetter([1, "A", 3, 4]) == null
    assert takeLetter([1, "B", 3, 4]) == null

    assert generateKey(createDeck(), 20) == "DWJXHYRFDGTMSHPUURXJ"
  }

  static generateKey(deck, size) {
    def result = ""
    while (result.length() < size) {
      deck = moveCard("A", 1, deck)
      deck = moveCard("B", 2, deck)
      deck = tripleCut(deck)
      deck = countCut(deck)
      def letter = takeLetter(deck)
      if (letter != null) result += letter
    }
    result
  }

  static takeLetter(deck) {
    def first = deck.first() == "A" || deck.first() == "B" ? 53 : deck.first()
    def card = deck[first]
    if (card == "A" || card == "B") null
    else (char) (card % 26) + 64
  }

  static countCut(deck) {
    def count = deck.last()
    if (deck.last() == "A" || deck.last() == "B") count = 53
    deck[count..-2] + deck[0..<count] + deck.last()
  }

  static tripleCut(deck) {
    def indexOfFirstJack = deck.findIndexOf {it == "A" || it == "B"}
    def indexOfLastJack = deck.findLastIndexOf {it == "A" || it == "B"}

    def beforeFirstJack = deck[0..<indexOfFirstJack]
    def betweenJacks = deck[indexOfFirstJack..indexOfLastJack]
    def afterLastJack = indexOfLastJack < deck.size() - 1 ? deck[indexOfLastJack + 1..-1] : []
    afterLastJack + betweenJacks + beforeFirstJack
  }

  static moveCard(card, moveSize, deck) {
    deck = new LinkedList(deck)
    def i = deck.indexOf(card)
    def newPosition = (i + moveSize) % deck.size()
    if (newPosition < i) newPosition += 1
    deck.remove((int) i)
    deck.add(newPosition, card)
    deck
  }

  static createDeck() {
    (1..52).toList() + "A" + "B"
  }

  @Test public void encodingDecoding() {
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
  }

  static decode(String message, String key) {
    message = message.replaceAll(/[\s]/, "")
    fromInts(subtract(asInts(message), asInts(key)))
  }

  static encode(String message, String key) {
    fromInts(sum(asInts(message), asInts(key)))
  }

  static fromInts(def ints) {
    ints.collect { (char) (it + 64) }.join("")
  }

  static subtract(List msgInts, List keyInts) {
    Collection.metaClass.mixin(GroovyUtil)
    msgInts.injectWithIndex([]) { acc, value, i ->
      def letter = value - keyInts[i % keyInts.size()]
      if (letter < 1) letter += 26 // was < 0 :(
      acc + letter
    }
  }

  static sum(List msgInts, List keyInts) {
    Collection.metaClass.mixin(GroovyUtil)
    msgInts.injectWithIndex([]) { acc, value, i ->
      def letter = value + keyInts[i % keyInts.size()]
      if (letter > 26) letter -= 26
      acc + letter
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
