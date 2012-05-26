package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source
import collection.immutable.SortedSet
import ru.util.Pomodoro

/**
 * User: dima
 * Date: 26/05/2012
 */

@Pomodoro("1")
class WordChain8 extends ShouldMatchers {
	@Test def shouldFindMinChainOfTransformationsFromOneWordToAnother() {
		findMinChain("aaa", "bbb", Set()) should equal(Seq())
		findMinChain("aaa", "aaa", Set()) should equal(Seq("aaa"))
		findMinChain("aaa", "abc", Set("aaa", "aba", "abc")) should equal(Seq("aaa", "aba", "abc"))
		findMinChain("aaa", "abc", Set("aaa", "aac", "aba", "abc")) should equal(Seq("aaa", "aba", "abc"))
	}

	@Test def shouldLoadStandardUnixDictionary() {
		val dictionary = loadDictionary()
		dictionary.size should equal(234371)
	}

	@Test def shouldDetermineIfWordsAreDifferentByJustOneCharacter() {
		// TODO
	}

	@Test def shouldFindMinWordChainFromCatToDog() {
		val dictionary = loadDictionary()
		findMinChain("cat", "dog", dictionary) should equal(Seq())
	}

	def loadDictionary(): Set[String] = {
		val words = Source.fromFile("/usr/share/dict/words").getLines().toSeq.map(_.toLowerCase)
		SortedSet(words: _*)
	}
	
	def findMinChain(fromWord: String, toWord: String, dictionary: Set[String]): Seq[String] = {
		if (fromWord.length != toWord.length) return Seq()
		val shortenedDictionary = dictionary.filter(_.length == fromWord.length)
		doFindMinChain(fromWord, toWord, shortenedDictionary - fromWord, Seq(fromWord), Int.MaxValue)
	}

	def doFindMinChain(fromWord: String, toWord: String, dictionary: Set[String],
	                   chain: Seq[String], minChainSize: Int): Seq[String] = {
		if (chain.size >= minChainSize) return Seq()
		if (fromWord == toWord) {
			println("Found " + chain.size + " " + chain)
			return chain
		}
		if (dictionary.isEmpty) return Seq()

		var minChain = Seq[String]()
		var min = minChainSize
		nextWords(fromWord, dictionary).foreach { word =>
			val newChain = doFindMinChain(word, toWord, dictionary - word, chain :+ word, min)
			if (!newChain.isEmpty) {
				minChain = newChain
				min = newChain.length
			}
		}
		minChain
	}

	def nextWords(word: String, dictionary: Set[String]): Set[String] = {
		dictionary.filter(canBeNextWord(word, _))
	}

	def canBeNextWord(fromWord: String, toWord: String) = {
		false
	}

}