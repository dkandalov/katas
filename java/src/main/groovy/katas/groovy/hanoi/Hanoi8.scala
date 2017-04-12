package katas.groovy.hanoi

import org.scalatest.Matchers
import org.junit.Test


class Hanoi8 extends Matchers {
	@Test def `find moves to solve Towers of Hanoi problem`() {
		findMoves(towerSize = 1).toList should equal(Seq(Move(0, -1)))
		findMoves(towerSize = 2).toList should equal(Seq(Move(0, 1), Move(1, -1), Move(0, 1)))
		findMoves(towerSize = 3).toList should equal(Seq(
			Move(0, -1), Move(1, 1), Move(0, -1),
			Move(2, -1),
			Move(0, -1), Move(1, 1), Move(0, -1)
		))
	}

	private case class Move(towerIndex: Int, shift: Int)

	private def findMoves(towerSize: Int): Stream[Move] = {
		def directionFor(towerIndex: Int): Int = {
			val multiple = if (towerSize % 2 == 0) 1 else -1
			if (towerIndex % 2 == 0) multiple else -multiple
		}
		def moves(towerSize: Int): Stream[Move] = {
			if (towerSize == 0) Stream()
			else {
				val subMoves = moves(towerSize - 1)
				val towerIndex = towerSize - 1
				subMoves #::: Stream(Move(towerIndex, directionFor(towerIndex))) #::: subMoves
			}
		}
		moves(towerSize)
	}
}