package katas.scala.classic_katas.wordchain

import org.junit.Test
import org.scalatest.Matchers

import scala.annotation.tailrec
import scala.io.Source

/**
 * User: dima
 * Date: 04/05/2012
 */

class WordChain3 extends Matchers {
	private val dict = Source.fromFile("/usr/share/dict/words").getLines().toList
	private var words = List[String]()
	private var minChain = List[String]()
	private var minChainSize = Int.MaxValue
	
	@Test def aaaa() {
		val wordChain = findShortestChain("cat", "dog")
		println(wordChain)
		wordChain should equal(List("cat", "cag", "cog", "dog"))
	}

	def findShortestChain(fromWord: String, toWord: String): List[String] = {
		words = dict.filter{ _.length == fromWord.length }.map{_.toLowerCase}
		findShortestChain(fromWord, toWord, List(fromWord))
	}

	private def findShortestChain(fromWord: String, toWord: String, wordChain: List[String]): List[String] = {
		if (fromWord == toWord) {
			minChain = wordChain
			minChainSize = minChain.size
			println(minChain.size + ": " + minChain)

			minChain
		} else if (wordChain.size + 1 >= minChainSize) {
			List[String]()
		} else {
			val nextWords = words.filter{canBeNextWord(fromWord, _)} diff wordChain
			nextWords.foreach { word =>
				if (wordChain.size + 1 < minChainSize)
					findShortestChain(word, toWord, wordChain :+ word)
			}
			minChain
		}
	}

	@tailrec private def canBeNextWord(fromWord: String, word: String, diffs: Int = 0): Boolean = {
		if (diffs > 1) false
		else if (fromWord.isEmpty && word.isEmpty) true
		else if (fromWord(0) != word(0)) canBeNextWord(fromWord.tail, word.tail, diffs + 1)
		else canBeNextWord(fromWord.tail, word.tail, diffs)
	}
}