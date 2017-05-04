package katas.scala.classic_katas.wordchain

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.io.Source

/**
 * User: dima
 * Date: 14/05/2012
 */

class WordChain4 extends ShouldMatchers {
	@Test def aaa() {
		val dictionary = Source.fromFile("/usr/share/dict/words").getLines().toSeq
		findMinChain("cat", "cat", dictionary) should equalTo(Seq("cat"))
		findMinChain("cat", "cot", dictionary) should equalTo(Seq("cat", "cot"))
//		findMinChain("cat", "dog", dictionary) should equalTo(Seq("cat", "dog"))
	}

	var minSize = Int.MaxValue

	def findMinChain(fromWord: String, toWord: String, dictionary: Seq[String]): Seq[String] = {
		if (fromWord.length() != toWord.length()) return Seq()
		if (fromWord == toWord) return Seq(fromWord)

		val words = dictionary.filter(_.length() == fromWord.length()).diff(Seq(fromWord))
		val minChain = doFindMinChain(fromWord, toWord, words)
		if (minChain.isEmpty) Seq() else fromWord +: minChain
	}

	def doFindMinChain(fromWord: String, toWord: String, dictionary: Seq[String]): Seq[String] = {
		if (dictionary.isEmpty) return Seq()
		if (fromWord == toWord) Seq(toWord)

		val result = Seq()
		dictionary.foreach { word =>
			if (canBeNext(word, toWord)) {
				val minChain = doFindMinChain(word, toWord, dictionary.diff(Seq(word)))
				if (minChain.isEmpty) fromWord +: minChain else Seq()
			}
		}
		result
	}

	def canBeNext(word: String, toWord: String): Boolean = {
		word.diff(toWord).size == 1
	}
}