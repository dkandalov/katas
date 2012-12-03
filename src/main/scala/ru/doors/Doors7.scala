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
	}

	@Test def sliding() {
		println(Seq(1,2,3,4,5,6,7,8).sliding(1, 3).toList)
	}

	private def visitDoors(amountOfDoors: Int): Seq[Boolean] = {
		def walk(stepSize: Int, doors: Seq[Boolean]): Seq[Boolean] = {
			if (stepSize > amountOfDoors) doors
			else {
				var newDoors = doors
				(-1 + stepSize).to(amountOfDoors, stepSize).foreach{ i => newDoors = newDoors.updated(i, !newDoors(i)) }
				walk(stepSize + 1, newDoors)
			}
		}

		val doors = Seq.fill(amountOfDoors) { false }
		walk(1, doors)
	}
}