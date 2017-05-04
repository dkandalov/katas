package katas.scala.textindex

import org.specs2.matcher.ShouldMatchers
import org.junit.Test


class TextIndex extends ShouldMatchers {

	@Test def `text index`() {
		new Index("abc").positionOf("a") should equalTo(0)
		new Index("abc").positionOf("ab") should equalTo(0)
		new Index("abc").positionOf("abc") should equalTo(0)

		new Index("abbc").positionOf("a") should equalTo(0)
		new Index("abbc").positionOf("b") should equalTo(2)
		new Index("abbc").positionOf("bc") should equalTo(2)

		new Index("cba").positionOf("a") should equalTo(2)
		new Index("cba").positionOf("ba") should equalTo(1)
		new Index("cba").positionOf("cb") should equalTo(0)
		new Index("cba").positionOf("ab") should equalTo(-1)
	}

	@Test def `realistic example`() {
		val text = "Then we can use binary search on this table. For example, to find out whether " +
			"the phrase never mind appears in this text, we compare with long"

		new Index(text).positionOf("apple") should equalTo(-1)
		new Index(text).positionOf("never mind") should equalTo(89)
	}

	private class Index(text: String) {
		private val index = Range(0, text.size).sortBy{ i => text.substring(i) }

		def positionOf(s: String): Int = {
			var from = 0
			var to = index.size

			while (from < to) {
				val midIndex = (from + to) / 2
				val midValue = text.substring(index(midIndex))
				val result = startsWithCompare(midValue, s)

				if (result == 0) return index(midIndex)
				else if (result < 0) from = midIndex + 1
				else to = midIndex
			}
			-1
		}

		private def startsWithCompare(text: String, s: String): Int = {
			if (s.isEmpty) 0
			else if (text.isEmpty) 1 // implies that longer strings are alphabetically "greater"
			else {
				val result = text.head.compareTo(s.head)
				if (result == 0) startsWithCompare(text.tail, s.tail) else result
			}
		}
	}
}