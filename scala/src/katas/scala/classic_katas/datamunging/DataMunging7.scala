package katas.scala.classic_katas.datamunging

import org.scalatest.Matchers
import org.junit.Test
import scala.io.Source

/**
 * User: dima
 * Date: 13/10/2012
 */

class DataMunging7 extends Matchers { // TODO try streaming implementation
	@Test def shouldFindDayWithMinTemperatureSpread() {
		val lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 8, 2)
		val dayWithMinTempSpread = convertToEntries(lines, 0, 1, 2).minBy{ _.valueDifference }.key

		lines.size should equal(30)
		dayWithMinTempSpread should equal("14")
	}

	@Test def shouldFindTeamWithMinGoalDifference() {
		val lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 5, 1)
		val teamWithMinGoalDiff = convertToEntries(lines, 1, 6, 8).minBy{ _.valueDifference }.key

		lines.size should equal(20)
		teamWithMinGoalDiff should equal("Aston_Villa")
	}

	private def readLines(fileName: String, dropLeft: Int, dropRight: Int): Seq[String] = {
		Source.fromFile(fileName).getLines().toSeq.drop(dropLeft).dropRight(dropRight).filterNot{ _.trim.matches("--+") }
	}

	private def convertToEntries(lines: Seq[String], keyIndex: Int, value1Index: Int, value2Index: Int): Seq[Entry] = {
		lines.map { line => line.trim.split("\\s+")}
			.map { split => Entry(split(keyIndex), toInt(split(value1Index)), toInt(split(value2Index))) }
	}

	case class Entry(key: String, value1: Int, value2: Int) {
		def valueDifference: Int = math.abs(value1 - value2)
	}

	private def toInt(s: String) = s.replace("*", "").toInt

}