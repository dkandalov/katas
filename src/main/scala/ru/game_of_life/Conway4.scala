package ru.game_of_life

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 27/10/2012
 */

class Conway4 extends ShouldMatchers {
	@Test def fieldShouldReturnCellState() {
		def field = new Field(
			"""
			  |0--
			  |-0-
			  |--0
			""")
		field.cellAt(0, 0) should equal("0")
		field.cellAt(0, 1) should equal("-")
		field.cellAt(0, 2) should equal("-")

		field.cellAt(1, 0) should equal("-")
		field.cellAt(1, 1) should equal("0")
		field.cellAt(1, 2) should equal("-")
	}

	class Field(s: String) {
		def cellAt(row: Int, col: Int): String = {
			"0"
		}
	}
}