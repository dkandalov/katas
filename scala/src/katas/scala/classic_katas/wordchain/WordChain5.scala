package katas.scala.classic_katas.wordchain

import org.junit.Test
import org.scalatest.Matchers

import scala.io.Source

/**
 * User: dima
 * Date: 15/05/2012
 */


class WordChain5 extends Matchers {
	@Test def aaa() {
		val dictionary: Seq[String] = Source.fromFile("/usr/share/dict/words").getLines().map(_.toLowerCase).toSet[String].toSeq
		findMinWordChain("cat", "dog", dictionary) should equal("cat", "other words", "dog")
	}
	
	def findMinWordChain(fromWord: String, toWord: String, dictionary: Seq[String]): Seq[String] = {
		if (fromWord.length() != toWord.length()) return Seq()
		val newDict = dictionary.filter(_.length() == fromWord.length()).diff(Seq(fromWord))
		doFindMinWordChain(fromWord, toWord, newDict, Seq(fromWord))
	}

	var chainsCache: Map[(String, String), Seq[String]] = Map()

	def doFindMinWordChain(fromWord: String, toWord: String, dictionary: Seq[String], wordChain: Seq[String]): Seq[String] = {
		val cachedChain = chainsCache.get((fromWord, toWord))
		if (!cachedChain.isEmpty && cachedChain.size < wordChain.size) return cachedChain.get

		if (fromWord == toWord) {
			println("found " + wordChain)
			chainsCache = chainsCache.updated((fromWord, toWord), wordChain)
			return wordChain
		}
		if (dictionary.isEmpty) return Seq()

		var result = Seq[String]()
		dictionary.foreach{ word =>
			if (canBeNext(fromWord, word)) {
				val newDict = dictionary.diff(Seq(word))
				result = doFindMinWordChain(word, toWord, newDict, wordChain :+ word)
			}
		}
		result
	}

	def findWordChains(fromWord: String, toWord: String, dictionary: Seq[String], wordChain: Seq[String]): Seq[Seq[String]] = {
		if (fromWord == toWord) {
			println("found " + wordChain)
			return Seq(wordChain)
		}
		if (dictionary.isEmpty) return Seq()

		var chains = Seq[Seq[String]]()
		dictionary.foreach{ word =>
			if (canBeNext(fromWord, word)) {
				val newDict = dictionary.diff(Seq(word))
				val subChains = findWordChains(word, toWord, newDict, wordChain :+ word)
				chains = subChains ++ chains
			}
		}
		chains
	}

	def canBeNext(word: String, nextWord: String): Boolean = word.diff(nextWord).size == 1
}