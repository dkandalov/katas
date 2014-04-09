package ru.katas.n19

import org.scalatest.Matchers
import org.junit.Test
import scala.io.Source
import collection.immutable.Set
import ru.util.{Arcade, Fail}

/**
 * User: dima
 * Date: 16/05/2012
 */

@Arcade @Fail
class WordChain6 extends Matchers {

	@Test def shouldFindShortestWordChain() {
		val dictionary = Source.fromFile("/usr/share/dict/words").getLines().map(_.toLowerCase).toSet
		findShortestWordChain("cat", "cat", dictionary) should equal(Seq("cat", "cat"))
		findShortestWordChain("cat", "dog", dictionary) should equal(Seq("cat", "other words", "dog"))
	}

	var chainCache: Map[(String, String), Seq[String]] = Map()
	var minChainSize: Int = Int.MaxValue
	
	def findShortestWordChain(fromWord: String, toWord: String, dictionary: Set[String]): Seq[String] = {
		val newDictionary = dictionary.filter(_.length() == fromWord.length()).diff(Set(fromWord))
		doFindShortestWordChain(fromWord, toWord, newDictionary, Seq(fromWord))
	}

	def doFindShortestWordChain(fromWord: String, toWord: String, dictionary: Set[String], chain: Seq[String]): Seq[String] = {
		if (chain.size > 20) return Seq()
		val cachedChain = chainCache.get((fromWord, toWord))
		if (cachedChain.isDefined && cachedChain.get.length <= chain.length + 1) return cachedChain.get
		if (fromWord == toWord) {
			val minChain = chain :+ toWord
			println("found " + minChain)
			chainCache = chainCache.updated((fromWord, toWord), minChain)
			return minChain
		}
		if (dictionary.isEmpty) return Seq()
		
		var result = Seq[String]()
		var dictCopy = dictionary
		dictionary.foreach { word =>
			if (canBeNext(fromWord, word)) {
				val subChain = doFindShortestWordChain(word, toWord, dictCopy.diff(Set(word)), chain :+ word)
				if (!subChain.isEmpty) {
					result = subChain
				} else {
					dictCopy = dictCopy.diff(Set(word))
				}
			}
		}
		result
	}
	
	def canBeNext(fromWord: String, nextWord: String): Boolean = fromWord.diff(nextWord).length() == 1
}