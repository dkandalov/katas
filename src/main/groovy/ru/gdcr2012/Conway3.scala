package ru.gdcr2012

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

class Conway3 extends ShouldMatchers {
	var isAlive: Boolean = true
	var amountOfNeighbours: Int = 0

	@Test def newCellShouldBeAlive() {
		isAlive = true
		isAlive should equal(true)
	}

	@Test def liveCellWithFewerThanTwoNeighboursShouldDie() {
		isAlive = true
		amountOfNeighbours = 1

		isAlive = !(amountOfNeighbours < 2)
		isAlive should equal(false)
	}

	@Test def liveCellWithTwoOrThreeLiveNeighboursShouldLive() {
		isAlive = true
		amountOfNeighbours = 2

		isAlive = (amountOfNeighbours == 2 || amountOfNeighbours == 3)
		isAlive should equal(true)
	}

	@Test def liveCellWithMoreThanThreeNeighboursShouldDie() {
		isAlive = true
		amountOfNeighbours = 4

		isAlive = amountOfNeighbours < 3
		isAlive should equal(false)
	}

	case class Cell(isAlive: Boolean, amountOfNeighbours: Int)

}