package ru.game_of_life

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 23/10/2012
 */

class Conway3 extends ShouldMatchers {
	@Test def whenAllCellsAreDeadNothingHappens() {
		new Field(
			"""
			  |---
			  |---
			  |---
			""").next() should equal(new Field("""
        |---
        |---
        |---
      """))
	}

	class Field(s: String) {
		def next(): Field = {
			this
		}

		override def equals(obj: Any) = super.equals(obj)
	}
}