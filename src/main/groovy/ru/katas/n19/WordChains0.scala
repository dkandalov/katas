package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source
import collection.immutable.TreeSet
import collection.immutable.Seq

/**
 * User: dima
 * Date: 22/04/2012
 */

class WordChains0 extends ShouldMatchers {
  // TODO finish

  @Test def aaa() {
    val words = Source.fromFile("/usr/share/dict/words").getLines().foldLeft(TreeSet[String]()) {
      (acc, word) =>
        acc + word.toLowerCase
    }
    println(
      findWordChain("cat", "dog", words)
    )
  }

  def findWordChain(fromWord: String, toWord: String, words: TreeSet[String]): Seq[String] = {
    if (!words.contains(fromWord) || !words.contains(toWord)) return Seq()
    findMinWordChain(fromWord, toWord, (words - fromWord).filter {
      _.length() == toWord.length()
    })
  }

  def findMinWordChain(fromWord: String, toWord: String, words: TreeSet[String],
                       length: Int = 0, minLength: Int = Int.MaxValue): Seq[String] = {
    if (length >= minLength) return Seq()
    if (fromWord == toWord) return {
      println("aa")
      Seq("")
    }

    val nextWords = words.foldLeft(TreeSet[String]()) {
      (acc, word) => if (canBeNext(fromWord, word)) acc + word else acc
    }
    println(nextWords.mkString(","))

    var newMinLength = minLength
    val updatedWords = words -- nextWords + toWord
    nextWords.foldLeft(Seq[String]()) {
      (acc, nextWord: String) =>
        val wordChain = findMinWordChain(nextWord, toWord, updatedWords, length + 1, newMinLength)
        if (!wordChain.isEmpty && wordChain.length < newMinLength) {
          println(wordChain)
          newMinLength = wordChain.length
          wordChain :+ nextWord
        } else {
          acc
        }
    }
  }

  @Test def shouldDetermineIfCanBeNextWord() {
    canBeNext("cat", "cad") should equal(true)
    canBeNext("cat", "cata") should equal(true)
    canBeNext("cat", "ct") should equal(true)

    canBeNext("cat", "cat") should equal(false)
    canBeNext("cat", "kata") should equal(false)
    canBeNext("cat", "ccc") should equal(false)
  }

  def canBeNext(fromWord: String, word: String) = {
    if (math.abs(fromWord.length() - word.length()) > 0) false
    else if (fromWord == word) false
    else {
      var diffs = 0
      var word1 = fromWord
      var word2 = word
      while ((!word1.isEmpty || !word2.isEmpty) && diffs <= 1) {
        //        println(word1)
        //        println(word2)
        //        println(diffs)

        if (word1.isEmpty) {
          word2 = word2.tail
          diffs += 1
        } else if (word2.isEmpty) {
          word1 = word1.tail
          diffs += 1
        } else if (word1.head == word2.head) {
          word1 = word1.tail
          word2 = word2.tail
        } else if (word1.size > 1 && word2.size > 1 && word1.tail.head == word2.tail.head) {
          word1 = word1.tail
          word2 = word2.tail
          diffs += 1
        } else if (word2.size > 1 && word1.head == word2.tail.head) {
          word2 = word2.tail
          diffs += 1
        } else if (word1.size > 1 && word1.tail.head == word2.head) {
          word1 = word1.tail
          diffs += 1
        } else {
          word1 = word1.tail
          word2 = word2.tail
          diffs += 1
        }
      }

      diffs <= 1
    }
  }
}