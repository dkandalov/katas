package ru.doors

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import collection.mutable.ArrayBuffer

/**
 * User: dima
 * Date: 03/09/2012
 */

class Doors6 extends ShouldMatchers {
	@Test def walkingDoors() {
		doors(20) should equal(ArrayBuffer(true, false, false, true, false, false, false, false, true,
			false, false, false, false, false, false, true, false, false, false, true))
	}

	private def doors(size: Int): Seq[Boolean] = {
		val doors = ArrayBuffer.fill(size) { false }

		for (step <- Range(1, size)) {
			for (i <- Range(step - 1, size, step)) {
				doors(i) = !doors(i)
			}
		}
		doors
	}
}