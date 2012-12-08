package ru.gdcr2012

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

class Conway3 extends ShouldMatchers {
	val aliveCell = new Cell(isAlive = true, amountOfNeighbours = 0)

	@Test def liveCellWithFewerThanTwoNeighboursShouldDieOnTheNextStep() {
		aliveCell.amountOfNeighbours = 1

		aliveCell.nextStep()
		aliveCell.isAlive should equal(false)
	}

	@Test def liveCellWithTwoOrThreeLiveNeighboursShouldLiveOnTheNextStep() {
		aliveCell.amountOfNeighbours = 2

//		aliveCell.isAlive = (aliveCell.amountOfNeighbours == 2 || aliveCell.amountOfNeighbours == 3)
		aliveCell.nextStep()
		aliveCell.isAlive should equal(true)
	}

	@Test def liveCellWithMoreThanThreeNeighboursShouldDieOnTheNextStep() {
		aliveCell.amountOfNeighbours = 4

		aliveCell.isAlive = aliveCell.amountOfNeighbours < 3
		aliveCell.isAlive should equal(false)
	}

	@Test def deadCellWithThreeNeighboursShouldBecomeLiveOnTheNextStep() {
		val deadCell = new Cell(isAlive = false, amountOfNeighbours = 3)
		deadCell.isAlive = deadCell.amountOfNeighbours == 3
		deadCell.isAlive should equal(true)
	}

	class Cell(var isAlive: Boolean, var amountOfNeighbours: Int) {
		def nextStep() {
			isAlive = !(aliveCell.amountOfNeighbours < 2)
		}
	}

}
