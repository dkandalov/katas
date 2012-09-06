package ru.connectivity

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import collection.mutable.ArrayBuffer
import annotation.tailrec

/**
 * User: dima
 * Date: 06/09/2012
 */

class WUnion extends ShouldMatchers {
	@Test def determineIfPointAreConnected_InSimplestCases() {
		val board = new Board(2)

		board.areConnected(0, 0) should equal(true)
		board.connect(0, 0)
		board.areConnected(0, 0) should equal(true)

		board.areConnected(0, 1) should equal(false)
		board.connect(0, 1)
		board.areConnected(0, 1) should equal(true)
	}

	@Test def determineIfPointsAreConnected_InSampleCase() {
		val input = Seq((3, 4), (4, 9), (8, 0), (2, 3), (5, 6),
			(2, 9), (5, 9), (7, 3), (4, 8), (5, 6), (0, 2), (6, 1))

		val board = new Board(10)
		val output = input.map { pair =>
			val wereConnected = board.areConnected(pair._1, pair._2)
			board.connect(pair._1, pair._2)
			wereConnected
		}

		output should equal(Seq(false, false, false, false, false,
			true, false, false, false, true, true, false))

		for (i <- 0 until 10; j <- 0 until 10) {
			board.areConnected(i, j) should equal(true)
		}
	}

	@Test def connectingPointSequentially() {
		val board = new Board(10)
		Range(0, 9).foreach { p =>
			board.connect(p, p + 1)
		}
		println(board.data)
	}

	class Board(size: Int) {
		val data = new ArrayBuffer[Int](0)
		data.insertAll(0, Range(0, size))

		def connect(p1: Int, p2: Int) {
			data(rootOf(p1)) = rootOf(p2)
		}

		def areConnected(p1: Int, p2: Int) = {
			rootOf(p1) == rootOf(p2)
		}

		@tailrec
		private def rootOf(p1: Int): Int = {
			if (data(p1) == p1) p1
			else rootOf(data(p1))
		}
	}
}