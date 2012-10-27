package ru.game_of_life

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 27/10/2012
 */

class Conway4 extends ShouldMatchers {
	@Test def fieldShouldReturnCellState() {
		new Field(
			"""
			  |
			""".stripMargin)
	}

	class Field(s: String) {
	}
}