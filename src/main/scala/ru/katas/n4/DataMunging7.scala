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
		val lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 8, 2)

		val entries = lines.map{ line => line.trim.split("\\s+") }.map{ split => Entry(split(0), toInt(split(1)), toInt(split(2))) }
		val dayWithMinTempSpread = entries.minBy{ entry => math.abs(entry.value1 - entry.value2) }.key

		lines.size should equal(30)
		entries.size should equal(30)
		entries(0) should equal(Entry("1", 88, 59))
		dayWithMinTempSpread should equal("14")
	}

	@Test def shouldFindTeamWithMinGoalDifference() {
		val lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 5, 1)

		val teamWithMinGoalDiff = convertToEntries(lines, 1, 6, 8).minBy{ entry => math.abs(entry.value1 - entry.value2) }.key

		lines.size should equal(20)
		teamWithMinGoalDiff should equal("Aston_Villa")
	}


	def convertToEntries(lines: Seq[String], keyIndex: Int, value1Index: Int, value2Index: Int): Seq[Entry] = {
		lines.map { line => line.trim.split("\\s+")}
			.map { split => Entry(split(keyIndex), toInt(split(value1Index)), toInt(split(value2Index))) }
	}

	private def readLines(fileName: String, dropLeft: Int, dropRight: Int): Seq[String] = {
		Source.fromFile(fileName).getLines().toSeq.drop(dropLeft).dropRight(dropRight).filterNot{ _.trim.matches("--+") }
	}

	case class Entry(key: String, value1: Int, value2: Int)

	private def toInt(s: String) = s.replace("*", "").toInt

}