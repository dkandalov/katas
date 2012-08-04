package ru.util

/**
 * User: dima
 * Date: 04/08/2012
 */

class Counter {
	private var i: Int = 0

	def count(threshold: Int = 10000, callback: Function1[Int, Unit]) {
		i += 1
		if (i % threshold == 0) callback(i)
	}
}