package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source

/**
 * User: dima
 * Date: 28/04/2012
 */

class WordChains1 extends ShouldMatchers {

  @Test def aaa() {
    val dictionary = Source.fromFile("/usr/share/dict/words").getLines().toSet[String]
//    val dictionary = Set("cat", "cot", "cog", "dog")
    println(wordChain("cat", "dog", dictionary))
  }

  def wordChain(fromWord: String, toWord: String, dictionary: Set[String]): Seq[String] = {
    val minChainSize = Int.MaxValue

    def findWordChains(fromWord_: String, toWord_: String, dictionary: Set[String]): Seq[Seq[String]] = {
      if (fromWord_ == toWord_) return Seq(Seq())
      println(dictionary.size)

      var result = Seq[Seq[String]]()
      dictionary.foreach { word =>
        if (canBeNext(fromWord_, word)) {
          val seq = findWordChains(word, toWord_, dictionary - word).map { (seq: Seq[String]) => seq :+ word }
          result = result ++ seq
        }
      }
      result
    }

    if (fromWord.length() != toWord.length()) {
      Seq()
    } else {
      val wordChains = findWordChains(fromWord, toWord, dictionary.filter { _.length == fromWord.length } - fromWord) // didn't remove fromWord
      if (wordChains.isEmpty) Seq() else (wordChains.minBy{ _.length } :+ fromWord).reverse // didn't add fromWord
    }
  }

  def canBeNext(fromWord: String, toWord: String, diffs: Int = 0): Boolean = {
    if (diffs > 1) false
    else if (fromWord.isEmpty) true // forgot empty check
    else if (fromWord(0) == toWord(0)) canBeNext(fromWord.tail, toWord.tail, diffs)
    else canBeNext(fromWord.tail, toWord.tail, diffs + 1)
  }
}