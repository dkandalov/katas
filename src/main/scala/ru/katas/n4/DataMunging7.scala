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

		def toInt(s: String) = s.replace("*", "").toInt
		val data = lines.map{ line => line.trim().split("\\s+") }.map{ split => (split(0), toInt(split(1)), toInt(split(2))) }

		lines.size should equal(30)
		data.size should equal(30)
		data(0) should equal(("1", 88, 59))
	}
}