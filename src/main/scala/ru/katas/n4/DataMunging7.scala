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

		val data = lines.map{ line => line.trim.split("\\s+") }.map{ split => (split(0), toInt(split(1)), toInt(split(2))) }
		val dayWithMinTempSpread = data.minBy{ entry => math.abs(entry._2 - entry._3) }._1

		lines.size should equal(30)
		data.size should equal(30)
		data(0) should equal(("1", 88, 59))
		dayWithMinTempSpread should equal("14")
	}

	@Test def shouldFindTeamWithMinGoalDifference() {
		val lines = Source.fromFile("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").getLines().toSeq
			.drop(5).dropRight(1).filterNot{ _.trim.matches("--+") }

		val data = lines.map{ line => line.trim.split("\\s+") }.map{ split => (split(1), toInt(split(6)), toInt(split(8))) }

		lines.size should equal(20)
		data.size should equal(20)
		data(0) should equal(("Arsenal", 79, 36))
	}

	private def toInt(s: String) = s.replace("*", "").toInt

}