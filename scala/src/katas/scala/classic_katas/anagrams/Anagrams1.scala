package katas.scala.classic_katas.anagrams

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.io.Source

/**
 * User: dima
 * Date: 06/12/2012
 */

class Anagrams1 extends ShouldMatchers {
	@Test def shouldReadDictionary() {
		val words = readDictionary
		words.size should equalTo(234371)
	}

	@Test def shouldTransformWordIntoCharacterCountMap() {
		characterCountOf("") should equalTo(Map())
		characterCountOf("aaa") should equalTo(Map('a' -> 3))
		characterCountOf("abab") should equalTo(Map('a' -> 2, 'b' -> 2))
		characterCountOf("cat") should equalTo(Map('c' -> 1, 'a' -> 1, 't' -> 1))
	}

	@Test def shouldGroupWordsByCharacterCount() {
		groupByCharCount(Seq()) should equalTo(Map())
		groupByCharCount(Seq("abba")) should equalTo(Map(Map('a' -> 2, 'b' -> 2) -> Seq("abba")))
		groupByCharCount(Seq("abba", "abab")) should equalTo(Map(Map('a' -> 2, 'b' -> 2) -> Seq("abba", "abab")))
	}

	@Test def shouldFindAnagrams() {
		val words = readDictionary
		groupByCharCount(words)
	}

	private def groupByCharCount(words: Seq[String]): Map[Map[Char, Int], Seq[String]] = words match {
		case Seq() => Map().withDefault{map => Seq[String]()}
		case head :: tail =>
			val map = groupByCharCount(tail)
			val charCount = characterCountOf(head)
			map.updated(charCount, head +: map(charCount))
	}

	private def readDictionary: Seq[String] = {
		Source.fromFile("/usr/share/dict/words").getLines().toList.map { _.toLowerCase }.toSet.toSeq
	}

	private def characterCountOf(s: String): Map[Char, Int] = {
		s.toList.foldLeft(Map[Char, Int]().withDefault{char => 0}) { (map, char) =>
			map.updated(char, map(char) + 1)
		}
	}
}