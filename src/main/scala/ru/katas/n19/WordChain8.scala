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

@Pomodoro("2.5")
class WordChain8 extends ShouldMatchers {
	@Test def shouldFindMinChainOfTransformationsFromOneWordToAnother() {
		findMinChain("aaa", "bbb", Set()) should equal(Seq())
		findMinChain("aaa", "aaa", Set()) should equal(Seq("aaa"))
		findMinChain("aaa", "abc", Set("aaa", "aba", "abc")) should equal(Seq("aaa", "aba", "abc"))
		findMinChain("aaa", "abc", Set("aaa", "aac", "aba", "abc")) should equal(Seq("aaa", "aac", "abc"))
	}

	@Test def shouldLoadStandardUnixDictionary() {
		val dictionary = loadDictionary()
		dictionary.size should equal(234371)
	}

	@Test def shouldDetermineIfWordsAreDifferentByJustOneCharacter() {
		canBeNextWord("cat", "car") should be(true)
		canBeNextWord("cat", "rca") should be(false)
		canBeNextWord("cat", "act") should be(false)
	}

	@Test def shouldFindMinWordChainFromCatToDog() {
		val dictionary = loadDictionary()
		findMinChain("cat", "dog", dictionary) should equal(Seq("cat", "cag", "cog", "dog"))
	}

	def loadDictionary(): Set[String] = {
		val words = Source.fromFile("/usr/share/dict/words").getLines().toSeq.map(_.toLowerCase)
		SortedSet(words: _*)
	}
	
	def findMinChain(fromWord: String, toWord: String, dictionary: Set[String]): Seq[String] = {
		if (fromWord.length != toWord.length) return Seq()
		val shortenedDictionary = dictionary.filter(_.length == fromWord.length)

		doFindMinChain(fromWord, toWord, shortenedDictionary - fromWord, 1, Int.MaxValue)
//		doFindMinChain_(fromWord, toWord, shortenedDictionary - fromWord, Seq(fromWord), Int.MaxValue)
	}

	// recursive process, recursive implementation
	def doFindMinChain(fromWord: String, toWord: String, dictionary: Set[String],
	                   chainSize: Int, minChainSize: Int): Seq[String] = {
		if (chainSize >= minChainSize) return Seq()
		if (fromWord == toWord) {
			println("Found " + chainSize)
			return Seq(toWord)
		}
		if (dictionary.isEmpty) return Seq()

		var minChain = Seq[String]()
		var min = minChainSize
		var updatedDictionary = dictionary

		nextWords(fromWord, dictionary).foreach { word =>
			updatedDictionary = updatedDictionary - word
			val newChain = doFindMinChain(word, toWord, updatedDictionary, chainSize + 1, min)
			if (!newChain.isEmpty) {
				minChain = fromWord +: newChain
				min = minChain.length + chainSize
			}
		}
		minChain
	}

	// iterative process, recursive implementation
	def doFindMinChain_(fromWord: String, toWord: String, dictionary: Set[String],
	                   chain: Seq[String], minChainSize: Int): Seq[String] = {
		if (chain.size >= minChainSize) return Seq()
		if (fromWord == toWord) {
			println("Found " + chain.size + " " + chain)
			return chain
		}
		if (dictionary.isEmpty) return Seq()

		var minChain = Seq[String]()
		var min = minChainSize
		var updatedDictionary = dictionary

		nextWords(fromWord, dictionary).foreach { word =>
			updatedDictionary = updatedDictionary - word
			val newChain = doFindMinChain_(word, toWord, updatedDictionary, chain :+ word, min)
			if (!newChain.isEmpty) {
				minChain = newChain
				min = minChain.length
			}
		}
		minChain
	}

	def nextWords(word: String, dictionary: Set[String]): Set[String] = {
		dictionary.filter(canBeNextWord(word, _))
	}

	def canBeNextWord(fromWord: String, toWord: String) = {
		var diffs = 0
		fromWord.corresponds(toWord) { (a,b) =>
			if (a != b) diffs += 1
			diffs <= 1 
		}
	}

}