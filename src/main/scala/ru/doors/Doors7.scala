package ru.doors

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 04/12/2012
 */

class Doors7 extends ShouldMatchers {
	@Test def shouldVisitDoors() {
		visitDoors(0) should equal(Seq())
		visitDoors(1) should equal(Seq(true))
		visitDoors(2) should equal(Seq(true, false))
		visitDoors(3) should equal(Seq(true, false, false))
		visitDoors(4) should equal(Seq(true, false, false, true))
	}

	private def visitDoors(amountOfDoors: Int): Seq[Boolean] = {
		def walk(stepSize: Int, position: Int, doors: Seq[Boolean]): Seq[Boolean] = {
			if (stepSize > amountOfDoors) doors
			else {
				var newDoors = doors
				(stepSize - 1).until(amountOfDoors, stepSize).foreach{ i => newDoors = newDoors.updated(i, !newDoors(i)) }
				walk(stepSize + 1, position, newDoors)
			}
		}

		val doors = Seq.fill(amountOfDoors) { false }
		walk(1, -1, doors)
	}
}