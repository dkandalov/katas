package katas.scala.classic_katas.datamunging

import org.junit.Test
import scala.io.Source
import org.specs2.matcher.ShouldMatchers

/**
 * User: dima
 * Date: 13/10/2012
 */

class DataMunging8 extends ShouldMatchers {

	@Test def shouldFindDayWithMinTemperatureSpread() {
		val lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 8, 2)
		val dayWithMinTempSpread = convertToEntries(lines, 0, 1, 2).minBy{ _.valueDifference }.key

		lines.size should equalTo(30)
		dayWithMinTempSpread should equalTo("14")
	}

	@Test def shouldFindTeamWithMinGoalDifference() {
		val lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 5, 1)
		val teamWithMinGoalDifference = convertToEntries(lines, 1, 6, 8).minBy{ _.valueDifference }.key

		lines.size should equalTo(20)
		teamWithMinGoalDifference should equalTo("Aston_Villa")
	}

	private def convertToEntries(lines: Seq[String], keyIndex: Int, value1Index: Int, value2Index: Int): Seq[Entry] = {
		lines.map { _.trim.split("\\s+")}.map { split =>
			Entry(split(keyIndex), toInt(split(value1Index)), toInt(split(value2Index)))
		}
	}

	private def readLines(fileName: String, dropLeft: Int, dropRight: Int): Seq[String] = {
		Source.fromFile(fileName).getLines().toSeq
			.drop(dropLeft).dropRight(dropRight).filterNot { _.trim.matches("--+") }
	}

	private def toInt(s: String): Int = s.replace("*", "").toInt

	case class Entry(key: String, value1: Int, value2: Int) {
		def valueDifference = math.abs(value1 - value2)
	}
}