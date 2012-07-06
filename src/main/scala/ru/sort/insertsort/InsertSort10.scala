package ru.sort.insertsort

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 06/07/2012
 */

class InsertSort10 extends ShouldMatchers {
	@Test def  aaa() {
		sort(Seq()) should equal(Seq())
		sort(Seq(1)) should equal(Seq(1))

		sort(Seq(1, 2)) should equal(Seq(1, 2))
		sort(Seq(2, 1)) should equal(Seq(1, 2))

		sort(Seq(1, 2, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 1, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 3, 1)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 2, 1)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 1, 2)) should equal(Seq(1, 2, 3))
	}

	def sort(seq: Seq[Int]): Seq[Int] = {
		Seq()
	}
}