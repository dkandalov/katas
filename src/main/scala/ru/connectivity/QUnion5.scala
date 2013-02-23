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
		val board = new Board(10)
		board.areConnected(0, 1) should equal(false)
		board.areConnected(1, 0) should equal(false)
		board.connect(0, 1) should equal(false)
		board.areConnected(0, 1) should equal(true)
		board.areConnected(1, 0) should equal(true)
	}

	@Test def exampleFromTheBook() {
		val board = new Board(10)
		val input = Seq((3, 4), (4, 9), (8, 0), (2, 3), (5, 6), (2, 9), (5, 9), (7, 3), (4, 8), (5, 6), (0, 2), (6, 1))
		val output = Seq(false, false, false, false, false, true, false, false, false, true, true, false)

		val actualOutput = input.map{ p => board.connect(p._1, p._2) }
		actualOutput should equal(output)
	}

	class Board(size: Int) {
		private val data: mutable.Buffer[Int] = Range(0, size).toBuffer

		def areConnected(p1: Int, p2: Int): Boolean = {
			rootOf(p1) == rootOf(p2)
		}

		def connect(p1: Int, p2: Int): Boolean = {
			val wereConnected = areConnected(p1, p2)
			data(rootOf(p1)) = rootOf(p2)
			wereConnected
		}

		private def rootOf(p: Int): Int = {
			if (data(p) == p) p
			else rootOf(data(p))
		}
	}
}