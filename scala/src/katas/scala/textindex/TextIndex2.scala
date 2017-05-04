package katas.scala.textindex

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class TextIndex2 extends ShouldMatchers {
	@Test def `indexed text`() {
		indexed("") should equalTo(List())
		indexed("abc") should equalTo(List(
			(0, "abc"), (1, "bc"), (2, "c")
		))
		indexed("cba") should equalTo(List(
			(2, "a"), (1, "ba"), (0, "cba")
		))
	}

	@Test def `finding position of string in indexed text`() {
		positionOf("", indexed("")) should equalTo(-1)
		positionOf("a", indexed("a")) should equalTo(0)

		positionOf("a", indexed("abc")) should equalTo(0)
		positionOf("b", indexed("abc")) should equalTo(1)
		positionOf("c", indexed("abc")) should equalTo(2)

		positionOf("a", indexed("cba")) should equalTo(2)
		positionOf("b", indexed("cba")) should equalTo(1)
		positionOf("c", indexed("cba")) should equalTo(0)
	}

	@Test def `realistic example`() {
		val text = "Then we can use binary search on this table. For example, to find out whether " +
			"the phrase never mind appears in this text, we compare with long"

		positionOf("apple", indexed(text)) should equalTo(-1)
		positionOf("never mind", indexed(text)) should equalTo(89)
	}


	private type Index = List[(Int, String)]

	private def positionOf(s: String, index: Index): Int = {
		def isLess(s1: String, s2: String): Boolean = {
			if (s1.isEmpty || s2.isEmpty) false
			else if (s1.head == s2.head) isLess(s1.tail, s2.tail)
			else s1.head < s2.head
		}

		var from = 0
		var to = index.length

		while (from < to) {
			val mid = (from + to) / 2
			val (position, text) = index(mid)

			if (text.startsWith(s)) {
				return position
			} else if (isLess(s, text)) {
				to = mid
			} else {
				from = mid + 1
			}
		}
		-1
	}

	private def indexed(text: String): Index = {
		Range(0, text.length).map{ i => (i, text.substring(i)) }.sortBy{_._2}.toList
	}

}