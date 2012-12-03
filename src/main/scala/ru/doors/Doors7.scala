package ru.doors

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 04/12/2012
 */

class Doors7 extends ShouldMatchers {
	@Test def shouldWalkDoors() {
		walkDoors(0) should equal(Seq())
		walkDoors(1) should equal(Seq(false))
	}

	private def walkDoors(amountOfDoors: Int): Seq[Boolean] = {
		val doors = Seq.fill(amountOfDoors){ true }
		Seq()
	}
}