package ru.katas.n4

import org.junit.Test
import scala.io.Source
import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 13/10/2012
 */

class DataMunging8 extends ShouldMatchers {
	@Test def shouldFindDayWithMinTemperatureSpread() {
		val lines = Source.fromFile("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").getLines().toSeq
			.drop(8).dropRight(2)
		val data = List(Entry("", 1, 1))

		lines.size should equal(30)
		data.size should equal(30)
	}

	case class Entry(key: String, value1: Int, value2: Int)
}