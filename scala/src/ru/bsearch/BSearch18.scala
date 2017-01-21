package ru.bsearch

import org.scalatest.Matchers
import org.junit.Test


class BSearch18 extends Matchers {
	@Test def bbb() {
		def search(n: Int, values: Seq[Int]): Int = {
//			if (values.isEmpty) {
//				return -1;
//			}

			//val isFound = false
			//while(!isFound) {}
			var from = 0
			var to = values.size
			while (from < to) {
				val midIndex = (from + to) / 2
				val midValue = values(midIndex)
				if (n < midValue) {
					to = midIndex
				} else if (n > midValue) {
					from = midIndex + 1
				} else {
					return midIndex
				}
			}
			-1
		}

		search(1, Seq()) should be(-1)

		search(0, Seq(1)) should be(-1)
		search(1, Seq(1)) should be(0)
		search(2, Seq(1)) should be(-1)

		search(0, Seq(1, 2)) should be(-1)
		search(1, Seq(1, 2)) should be(0)
		search(2, Seq(1, 2)) should be(1)
		search(3, Seq(1, 2)) should be(-1)

		search(0, Seq(1, 2, 3)) should be(-1)
		search(1, Seq(1, 2, 3)) should be(0)
		search(2, Seq(1, 2, 3)) should be(1)
		search(3, Seq(1, 2, 3)) should be(2)
		search(4, Seq(1, 2, 3)) should be(-1)

		search(0, Seq(1, 2, 3, 4)) should be(-1)
		search(1, Seq(1, 2, 3, 4)) should be(0)
		search(2, Seq(1, 2, 3, 4)) should be(1)
		search(3, Seq(1, 2, 3, 4)) should be(2)
		search(4, Seq(1, 2, 3, 4)) should be(3)
		search(5, Seq(1, 2, 3, 4)) should be(-1)
	}

	@Test def aa() {
		def search(n: Int, values: Seq[Int], shift: Int = 0): Int = {
			if (values.isEmpty) {
				return -1
			}
			val midIndex = values.size / 2
			val midValue = values(midIndex)
			if (n < midValue) {
				search(n, values.take(midIndex), shift)
			} else if (n > midValue) {
				search(n, values.takeRight(midIndex), shift + midIndex + (midIndex % 2))
			} else {
				midIndex + shift
			}
		}
		search(1, Seq()) should be(-1)

		search(0, Seq(1)) should be(-1)
		search(1, Seq(1)) should be(0)
		search(2, Seq(1)) should be(-1)

		search(0, Seq(1, 2)) should be(-1)
		search(1, Seq(1, 2)) should be(0)
		search(2, Seq(1, 2)) should be(1)
		search(3, Seq(1, 2)) should be(-1)

		search(0, Seq(1, 2, 3)) should be(-1)
		search(1, Seq(1, 2, 3)) should be(0)
		search(2, Seq(1, 2, 3)) should be(1)
		search(3, Seq(1, 2, 3)) should be(2)
		search(4, Seq(1, 2, 3)) should be(-1)

		search(0, Seq(1, 2, 3, 4)) should be(-1)
		search(1, Seq(1, 2, 3, 4)) should be(0)
		search(2, Seq(1, 2, 3, 4)) should be(1)
		search(3, Seq(1, 2, 3, 4)) should be(2)
		search(4, Seq(1, 2, 3, 4)) should be(3)
		search(5, Seq(1, 2, 3, 4)) should be(-1)
	}

}