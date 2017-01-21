package ru.connectivity

import org.junit.Test
import collection.mutable.ArrayBuffer
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 07/09/2012
 */

class QFind5 extends Matchers {
	@Test def determineIfPointsAreConnected_InSimpleCase() {
		val board = new Board(2)
		board.connected(0, 0) should equal(true)

		board.connected(0, 1) should equal(false)
		board.connect(0, 1)
		board.connected(0, 1) should equal(true)
	}

	@Test def determineIfPointsAreConnected_InSampleCase() {
		val board = new Board(10)

		def connect(p1: Int, p2: Int) = {
			val wereConnected = board.connected(p1, p2)
			board.connect(p1, p2)
			wereConnected
		}

		connect(3, 4) should equal(false)
		connect(4, 9) should equal(false)
		connect(8, 0) should equal(false)
		connect(2, 3) should equal(false)
		connect(5, 6) should equal(false)
		connect(2, 9) should equal(true)
		connect(5, 9) should equal(false)
		connect(7, 3) should equal(false)
		connect(4, 8) should equal(false)
		connect(5, 6) should equal(true)
		connect(0, 2) should equal(true)
		connect(6, 1) should equal(false)

		for (i <- 0 until 10; j <- 0 until 10) {
			board.connected(i, j) should equal(true)
		}
	}

	class Board(size: Int) {
		val data: ArrayBuffer[Int] = new ArrayBuffer[Int]()
		data.insertAll(0, Range(0, size))

		def connected(p1: Int, p2: Int) = {
			data(p1) == data(p2)
		}

		def connect(p1: Int, p2: Int) {
			val p1Root = data(p1)
			for (i <- 0 until data.size) {
				if (data(i) == p1Root) {
					data(i) = data(p2)
				}
			}
		}
	}
}