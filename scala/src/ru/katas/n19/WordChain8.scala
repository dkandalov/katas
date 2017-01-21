package ru.katas.n19

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.immutable.SortedSet
import scala.io.Source

/**
 * User: dima
 * Date: 26/05/2012
 */

class WordChain8 extends Matchers {
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

		case class State(minChain: Seq[String], min: Int, dict: Set[String])

		val seqop = { (state: State, word: String) =>
				val updatedDict = state.dict - word
				val newChain = doFindMinChain(word, toWord, updatedDict, chainSize + 1, state.min)
				if (!newChain.isEmpty) {
					val minChain = fromWord +: newChain
					State(minChain, minChain.length + chainSize - 1, updatedDict)
				} else {
					state
				}
		}
		val combine = { (state1: State, state2: State) =>
				if (state1.min < state2.min) state1 else state2
		}
		nextWords(fromWord, dictionary).par.aggregate(State(Seq[String](), minChainSize, dictionary))(seqop, combine).minChain
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