package ru.hanoi

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test


class Hanoi6 extends ShouldMatchers {
	@Test def `find moves to solve towers of Hanoi problem`() {
		findMoves(towerSize = 1) should equal(Seq(Move(0, -1)))
		findMoves(towerSize = 2) should equal(Seq(
			Move(0, 1), Move(1, -1), Move(0, 1)
		))
		findMoves(towerSize = 3) should equal(Seq(
			Move(0, -1), Move(1, 1), Move(0, -1),
			Move(2, -1),
			Move(0, -1), Move(1, 1), Move(0, -1)
		))
	}

	case class Move(ringIndex: Int, shift: Int)

	private def findMoves(towerSize: Int): Seq[Move] = {
		var moves = Seq[Move]()
		var ringIndex = towerSize - 1
		var shift = -1
		while (ringIndex >= 0) {
			moves = moves :+ Move(ringIndex, shift)
			ringIndex = ringIndex - 1
			shift = shift * -1
		}
		moves.foldRight(Seq[Move]()) { (move, result) =>
			result ++ Seq(move) ++ result
		}
	}

	private def findMoves_(towerSize: Int, shift: Int = -1): Seq[Move] = {
		if (towerSize == 0) Seq()
		else {
			val ringIndex = towerSize - 1
			val moves = findMoves_(towerSize - 1, -shift)
			moves ++ Seq(Move(ringIndex, shift)) ++ moves
		}
	}
}