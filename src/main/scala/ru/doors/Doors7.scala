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
		def walk(step: Int, doors: Seq[Boolean]): Seq[Boolean] = {
			if (step > amountOfDoors) doors
			else walk(step + 1, doors.sliding(1, step).flatten.toSeq)
		}

		val doors = Seq.fill(amountOfDoors) { false }
		walk(1, doors)
	}
}