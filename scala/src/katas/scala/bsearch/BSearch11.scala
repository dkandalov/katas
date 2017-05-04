package katas.scala.bsearch

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.annotation.tailrec

/**
 * User: dima
 * Date: 06/06/2012
 */

class BSearch11 extends ShouldMatchers {
	@Test def shouldFindIndexOfElementUsingBinarySearch() {
		find(1, Seq()) should equalTo(None)

		find(0, Seq(1)) should equalTo(None)
		find(1, Seq(1)) should equalTo(Some(0))
		find(2, Seq(1)) should equalTo(None)

		find(0, Seq(1, 2)) should equalTo(None)
		find(1, Seq(1, 2)) should equalTo(Some(0))
		find(2, Seq(1, 2)) should equalTo(Some(1))
		find(3, Seq(1, 2)) should equalTo(None)

		find(0, Seq(1, 2, 3)) should equalTo(None)
		find(1, Seq(1, 2, 3)) should equalTo(Some(0))
		find(2, Seq(1, 2, 3)) should equalTo(Some(1))
		find(3, Seq(1, 2, 3)) should equalTo(Some(2))
		find(4, Seq(1, 2, 3)) should equalTo(None)

		find(0, Seq(1, 2, 3, 4)) should equalTo(None)
		find(1, Seq(1, 2, 3, 4)) should equalTo(Some(0))
		find(2, Seq(1, 2, 3, 4)) should equalTo(Some(1))
		find(3, Seq(1, 2, 3, 4)) should equalTo(Some(2))
		find(4, Seq(1, 2, 3, 4)) should equalTo(Some(3))
		find(5, Seq(1, 2, 3, 4)) should equalTo(None)
	}

	@tailrec private def find(value: Int, values: Seq[Int], shift: Int = 0): Option[Int] = {
		if (values.isEmpty) return None

		val mid = values.size / 2
		val midValue = values(mid)

		if (midValue == value) Some(shift + mid)
		else if (value < midValue) find(value, values.slice(0, mid), shift)
		else find(value, values.slice(mid + 1, values.size), shift + mid + 1) // forgot "+ 1" in shift
	}
}