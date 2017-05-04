package katas.scala.connectivity

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.collection.mutable.ArrayBuffer

/**
 * User: dima
 * Date: 04/09/2012
 */

class QFind4 extends ShouldMatchers {
	@Test def determineIfPointsAreConnected_InSimpleCase() {
		val board = new Board(10)
		board.connected(0, 0) should equalTo(true)

		board.connected(0, 1) should equalTo(false)
		board.connect(0, 1)
		board.connected(0, 1) should equalTo(true)
	}

	@Test def determineIfPointsAreConnected_InSampleCase() {
		val input = Seq((3, 4), (4, 9), (8, 0), (2, 3), (5, 6),
			(2, 9), (5, 9), (7, 3), (4, 8), (5, 6), (0, 2), (6, 1))

		val board = new Board(10)
		val output = input.map { pair =>
			val wereConnected = board.connected(pair._1, pair._2)
			board.connect(pair._1, pair._2)
			wereConnected
		}

		output should equalTo(Seq(false, false, false, false, false,
			true, false, false, false, true, true, false))

		for (i <- 0 until 10; j <- 0 until 10) {
			board.connected(i, j) should equalTo(true)
		}
	}

	class Board(size: Int) {
		val data: ArrayBuffer[Int] = new ArrayBuffer[Int]()
		data.insertAll(0, Range(0, size))

		def connected(p1: Int, p2: Int): Boolean = {
			data(p1) == data(p2)
		}

		def connect(p1: Int, p2: Int) {
			val p1Root = rootOf(p1, data)
			val p2Root = rootOf(p2, data)

			for (i <- (0 until data.size)) {
				if (data(i) == p1Root) {
					data(i) = p2Root
				}
			}
		}
	}

	private def rootOf(p: Int, data: ArrayBuffer[Int]): Int = {
		var root = data(p)
		while (data(root) != root) {
			root = data(root)
		}
		root
	}
}