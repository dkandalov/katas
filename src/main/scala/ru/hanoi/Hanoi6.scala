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

	case class Move(tower: Int, shift: Int)

	private def findMoves(towerSize: Int, direction: Int = -1): Seq[Move] = {
		if (towerSize == 0) Seq()
		else {
			val ringIndex = towerSize - 1
			val moves = findMoves(towerSize - 1, -direction)
			moves ++ Seq(Move(ringIndex, direction)) ++ moves
		}
	}
}