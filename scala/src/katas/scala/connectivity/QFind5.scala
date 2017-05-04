package katas.scala.connectivity

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.collection.mutable.ArrayBuffer

/**
 * User: dima
 * Date: 07/09/2012
 */

class QFind5 extends ShouldMatchers {
	@Test def determineIfPointsAreConnected_InSimpleCase() {
		val board = new Board(2)
		board.connected(0, 0) should equalTo(true)

		board.connected(0, 1) should equalTo(false)
		board.connect(0, 1)
		board.connected(0, 1) should equalTo(true)
	}

	@Test def determineIfPointsAreConnected_InSampleCase() {
		val board = new Board(10)

		def connect(p1: Int, p2: Int) = {
			val wereConnected = board.connected(p1, p2)
			board.connect(p1, p2)
			wereConnected
		}

		connect(3, 4) should equalTo(false)
		connect(4, 9) should equalTo(false)
		connect(8, 0) should equalTo(false)
		connect(2, 3) should equalTo(false)
		connect(5, 6) should equalTo(false)
		connect(2, 9) should equalTo(true)
		connect(5, 9) should equalTo(false)
		connect(7, 3) should equalTo(false)
		connect(4, 8) should equalTo(false)
		connect(5, 6) should equalTo(true)
		connect(0, 2) should equalTo(true)
		connect(6, 1) should equalTo(false)

		for (i <- 0 until 10; j <- 0 until 10) {
			board.connected(i, j) should equalTo(true)
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