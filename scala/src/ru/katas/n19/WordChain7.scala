package ru.katas.n19

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.immutable.SortedSet
import scala.io.Source

/**
 * User: dima
 * Date: 24/05/2012
 */

class WordChain7 extends Matchers {
	@Test def shouldChainOneWordToAnotherUsingOneLetterModification() {
		findMinChain("aaa", "aaaa", SortedSet()) should equal(Seq())
		findMinChain("aaa", "abc", SortedSet("aaa")) should equal(Seq())
		findMinChain("aaa", "aaa", SortedSet("aaa")) should equal(Seq("aaa"))

		val dictionary = SortedSet("aaa", "aba", "abc")
		findMinChain("aaa", "abc", dictionary) should equal(Seq("aaa", "aba", "abc"))
	}

	@Test def shouldLoadDictionaryFromFileAsSortedSet() {
		val dictionary = loadDictionary()
		dictionary.size should equal(234371)
	}

	@Test def shouldFindMinimalChainFromCatToDog() {
		findMinChain("cat", "dog", loadDictionary()) should equal("") // TODO
	}

	def loadDictionary(): SortedSet[String] = {
		val words = Source.fromFile("/usr/share/dict/words").getLines().toSeq.map(_.toLowerCase)
		SortedSet(words: _*)
	}

	def findMinChain(fromWord: String, toWord: String, dictionary: Set[String]): Seq[String] = {
		if (fromWord.length != toWord.length) return Seq()
		if (dictionary.isEmpty) return Seq()

		val shortenedDict = dictionary.filter(_.size == fromWord.size)
		doFindMinChain(fromWord, toWord, shortenedDict - fromWord, Seq(fromWord), Int.MaxValue)
	}

	def doFindMinChain(fromWord: String, toWord: String, dictionary: Set[String], 
	                   chain: Seq[String], minChainSize: Int): Seq[String] = {
		if (fromWord == toWord) return chain
		if (dictionary.isEmpty) return Seq()
		
		var min = minChainSize
		var minChain = Seq[String]()
		nextWords(fromWord, dictionary).foreach { word =>
			val newChain = doFindMinChain(word, toWord, dictionary - word, chain :+ word, min)
			if (newChain.size > 0 && newChain.size < min) {
				minChain = newChain
				min = newChain.size
			}
		}
		
		minChain
	}
	
	def nextWords(fromWord: String, dictionary: Set[String]): Set[String] = {
		dictionary.filter{ word => word.diff(fromWord).size == 1 }
	}

}