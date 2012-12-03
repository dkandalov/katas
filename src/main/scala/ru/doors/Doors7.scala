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
		walkDoors(1) should equal(Seq(true))
		walkDoors(2) should equal(Seq(true, false))
	}

	private def walkDoors(amountOfDoors: Int): Seq[Boolean] = {
		def walk(step: Int, doors: Seq[Boolean]): Seq[Boolean] = {
			if (step > amountOfDoors) doors
			else walk(step + 1, doors)
		}

		walk(1, Seq.fill(amountOfDoors){false})
	}
}