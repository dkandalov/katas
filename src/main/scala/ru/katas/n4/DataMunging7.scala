package ru.katas.n4

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source

/**
 * User: dima
 * Date: 13/10/2012
 */

class DataMunging7 extends ShouldMatchers {
	@Test def shouldFindDayWithMinTemperatureSpread() {
		val lines = Source.fromFile("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").getLines().toSeq
			.drop(8).dropRight(2)
		val data = List((""))

		lines.size should equal(30)
		data.size should equal(30)
		data(0)(0) should equal("1")
	}
}