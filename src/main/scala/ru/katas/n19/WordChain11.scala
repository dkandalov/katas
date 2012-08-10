package ru.katas.n19

import org.junit.Test
import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 10/08/2012
 */

class WordChain11 extends ShouldMatchers {

	@Test def shouldFindSimpleWordChains() {
		findShortestWordChain("aaa", "aaa", Seq("aaa")) should equal(Seq("aaa"))
		findShortestWordChain("aaa", "ccc", Seq("aaa", "ccc")) should equal(Seq())
		findShortestWordChain("aaa", "ccc", Seq("aaa", "aac", "acc", "ccc")) should equal(Seq("aaa", "aac", "acc", "ccc"))
	}

	def findShortestWordChain(fromWord: String, toWord: String, dict: Seq[String]): Seq[String] = {
		if (fromWord == toWord) return Seq(toWord)

		val nextWords = dict.filter{ canBeNext(fromWord, _) }
		var result = Seq()
		for (word <- nextWords) {

		}

		result
	}

	@Test def shouldFindNextWords() {
		canBeNext("aaa", "aaa") should be(false)
		canBeNext("aaa", "aab") should be(true)
		canBeNext("aaa", "aba") should be(true)
		canBeNext("aaa", "baa") should be(true)
		canBeNext("aaa", "bba") should be(false)
	}

	private def canBeNext(fromWord: String, toWord: String): Boolean = {
		fromWord.diff(toWord).size == 1
	}
}