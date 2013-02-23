package ru.connectivity

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import collection.mutable

/**
 * User: dima
 * Date: 23/02/2013
 */

class QUnion5 extends ShouldMatchers {
	@Test def connectingTwoPoints() {
		val qUnion = new QUnion(10)
		qUnion.areConnected(0, 1) should equal(false)
		qUnion.connect(0, 1) should equal(false)
		qUnion.areConnected(0, 1) should equal(true)
	}

	class QUnion(size: Int) {
		private val data: mutable.Buffer[Int] = Range(0, size).toBuffer

		def areConnected(p1: Int, p2: Int): Boolean = {
			rootOf(p1) == rootOf(p2)
		}

		private def rootOf(p: Int): Int = {
			if (data(p) == p) p
			else rootOf(data(p))
		}

		def connect(p1: Int, p2: Int): Boolean = {
			val wereConnected = areConnected(p1, p2)
			data(p1) = p2
			wereConnected
		}
	}
}