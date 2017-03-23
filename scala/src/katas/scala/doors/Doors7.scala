package katas.scala.doors

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 04/12/2012
 */

class Doors7 extends Matchers {
	@Test def shouldVisitDoors() {
		visitDoors(0) should equal(Seq())
		visitDoors(1) should equal(Seq(true))
		visitDoors(2) should equal(Seq(true, false))
		visitDoors(3) should equal(Seq(true, false, false))
		visitDoors(4) should equal(Seq(true, false, false, true))
	}

	private def visitDoors(amountOfDoors: Int): Seq[Boolean] = {
		def walk(stepSize: Int, doors: Seq[Boolean]): Seq[Boolean] = {
			if (stepSize > amountOfDoors) doors
			else {
				var newDoors = doors
				(stepSize - 1).until(amountOfDoors, stepSize).foreach{ i => newDoors = newDoors.updated(i, !newDoors(i)) }
				walk(stepSize + 1, newDoors)
			}
		}

		val doors = Seq.fill(amountOfDoors) { false }
		walk(1, doors)
	}
}