package ru.bsearch

import org.junit.Test
import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 18/06/2012
 */

class BSearch13 extends ShouldMatchers {
	@Test def should_find_element_index_in_a_sequence() {
		bsearch(1, Seq()) should equal(None)

		bsearch(0, Seq(1)) should equal(None)
		bsearch(1, Seq(1)) should equal(Some(0))
		bsearch(2, Seq(1)) should equal(None)

		bsearch(0, Seq(1, 2)) should equal(None)
		bsearch(1, Seq(1, 2)) should equal(Some(0))
		bsearch(2, Seq(1, 2)) should equal(Some(1))
		bsearch(3, Seq(1, 2)) should equal(None)

		bsearch(0, Seq(1, 2, 3)) should equal(None)
		bsearch(1, Seq(1, 2, 3)) should equal(Some(0))
		bsearch(2, Seq(1, 2, 3)) should equal(Some(1))
		bsearch(3, Seq(1, 2, 3)) should equal(Some(2))
		bsearch(4, Seq(1, 2, 3)) should equal(None)
	}

	case class State(n: Int, values: Seq[Int], from: Int, to: Int, result: Option[Option[Int]])

	def bsearch(n: Int, values: Seq[Int]): Option[Int] = {
		doBinarySearch(n, values, 0, values.size).result.get
	}

	def doBinarySearch(n: Int, values: Seq[Int], from: Int, to: Int): State = {
		null
	}
}