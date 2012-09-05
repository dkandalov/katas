package ru.connectivity

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import collection.mutable.ArrayBuffer
import annotation.tailrec

/**
 * User: dima
 * Date: 05/09/2012
 */

class QUnion4 extends ShouldMatchers {
	@Test def pointsAreConnected_InSimpleCases() {
		var board = new Board(2)

		board.connected(0, 0) should equal(true)

		board.connected(0, 1) should equal(false)
		board.connected(1, 0) should equal(false)
		board = board.connect(0, 1)
		board.connected(0, 1) should equal(true)
		board.connected(1, 0) should equal(true)
	}

	@Test def pointsAreConnected_InSampleCase() {
		val input = Seq(
			(3, 4), (4, 9), (8, 0), (2, 3), (5, 6), (2, 9), (5, 9), (7, 3), (4, 8), (5, 6), (0, 2), (6, 1)
		)

		var board = new Board(10)
		val output = input.map { pair =>
			val wereConnected = board.connected(pair._1, pair._2)
			board = board.connect(pair._1, pair._2)
			wereConnected
		}

		output should equal(
			Seq(false, false, false, false, false, true, false, false, false, true, true, false)
		)

		for (i <- 0 until 10; j <- 0 until 10) {
			board.connected(i, j) should equal(true)
		}
	}

	class Board() {
		var data: Seq[Int] = _

		def this(data: Seq[Int]) = {
			this()
			this.data = data
		}

		def this(size: Int) = {
			this()
			this.data = Seq(Range(0, size): _ *)
		}

		def connect(p1: Int, p2: Int): Board = {
			new Board(data.updated(rootOf(p1), rootOf(p2)))
		}

		def connected(p1: Int, p2: Int) = {
			rootOf(p1) == rootOf(p2)
		}

		@tailrec private def rootOf(p: Int): Int = {
			if (data(p) == p) p
			else rootOf(data(p))
		}
	}
}