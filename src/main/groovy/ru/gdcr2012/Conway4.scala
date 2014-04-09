package ru.gdcr2012

import org.scalatest.Matchers
import org.junit.Test

class Conway4 extends Matchers {
//	@Test def /*aa*/emptyBoardShouldRem/*e*/ainEmpty() {
//	@Test def emptyBoardShouldRemainEmpty() {
	@Test def live_cellWithNoNeighboursShouldDieOnTheNextStep() {
		val allCells = List()
		Cell(0, 0, isAlive = true).nextStep(allCells) should equal(Cell(0, 0, isAlive = false))
	}

//	@Test def cellWithNoNeighboursShouldDieOnTheNextStep() {
	@Test def live_cellWithOneNeighboursShouldDieOnTHeNextStep() {
		val allCells = List(Cell(0, 1, isAlive = true))
		Cell(0, 0, isAlive = true).nextStep(allCells) should equal(Cell(0, 0, isAlive = false))
	}

	@Test def live_cellWithTwoNeighboursShouldRemainAliveOnTheNextStep() {
		val allCells = List(Cell(0, 1, isAlive = true), Cell(1, 0, isAlive = true))
		Cell(0, 0, isAlive = true).nextStep(allCells) should equal(Cell(0, 0, isAlive = true))//_
	}

	@Test def dead_cellWithTwoNeoighbioursDiesOnTheNextStep() {//live_cellWithTwoNeighboursShouldRemainAliveOnTheNextStep() {
		val allCells = List(Cell(0, 1, isAlive = true), Cell(1, 0, isAlive = true))
		Cell(0, 0, isAlive = false/*true*/).nextStep(allCells) should equal(Cell(0, 0, isAlive = false/*fl*//*flase*//*true*/))//_
	}

//	@tes
	@Test def live_cellWithThreeNeighboursLivesOnTheNextStep() {
		val allCells = List(Cell(0, 1, isAlive = true), Cell(1/*0*/, 1, isAlive = true), Cell(1/*0*/, 0/*1*/, isAlive = true))
		Cell(0, 0, isAlive = true).nextStep(allCells) should equal(Cell(0, 0, isAlive = true/*false*/))

	}
//)()//s//ToThe

	case class Cell(x:Int, y:Int, isAlive:/*bool*//*:*/Boolean) {
		def nextStep(cells/*value*/: List[/*Nothing*/Cell]): Cell = {
			//Cell(x, y, /*isAlive*/false)
//			val amountOfNeighnocells.filter{_.isAlive}.size //> //() {_.}
			val amountOfNeighbours = cells.filter{_.isAlive}.size
			if (isAlive) {//}
//				val amountOfNeighbours = cells.filter{_.isAlive}.size
				Cell(x, y, amountOfNeighbours >= 2 || amountOfNeighbours == 3) // hahahaha!!!!
				//|
			} else {
				Cell(x, y, amountOfNeighbours == 3)
			}
		}
	}

	class Board {

	}
}