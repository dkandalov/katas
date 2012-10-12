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

		lines.size should equal(20)
	}
}