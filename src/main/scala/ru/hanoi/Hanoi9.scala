package ru.hanoi

import org.scalatest.Matchers
import org.junit.Test


class Hanoi9 extends Matchers {
	@Test def `find moves for Towers of Hanoi problem`() {
		findMoves_(sizeOfTower = 0) should equal(Seq())
		findMoves_(sizeOfTower = 1) should equal(Seq(Move(0, -1)))
		findMoves_(sizeOfTower = 2) should equal(Seq(Move(0, 1), Move(1, -1), Move(0, 1)))
		findMoves_(sizeOfTower = 3) should equal(Seq(
			Move(0, -1), Move(1, 1), Move(0, -1),
			Move(2, -1),
			Move(0, -1), Move(1, 1), Move(0, -1)
		))
	}

	case class Move(towerIndex: Int, shift: Int)

	private implicit def streamAsList[T](s: Stream[T]) = s.toList

	def findMoves(sizeOfTower: Int, shift: Int = -1): Seq[Move] = {
		if (sizeOfTower == 0) Seq()
		else {
			val subMoves = findMoves(sizeOfTower - 1, -shift)
			val towerIndex = sizeOfTower - 1
			subMoves ++ Seq(Move(towerIndex, shift)) ++ subMoves
		}
	}

	def findMoves_(sizeOfTower: Int, shift: Int = -1): Stream[Move] = {
		if (sizeOfTower == 0) Stream()
		else {
			val subMoves = findMoves_(sizeOfTower - 1, -shift)
			val towerIndex = sizeOfTower - 1
			subMoves #::: Stream(Move(towerIndex, shift)) #::: subMoves
		}
	}
}