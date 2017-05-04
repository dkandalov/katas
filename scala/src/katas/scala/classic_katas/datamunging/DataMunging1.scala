package katas.scala.classic_katas.datamunging

import org.specs2.matcher.ShouldMatchers
import scala.io.Source
import org.hamcrest.Matchers._
import org.junit.{Assert, Test}
import collection.Seq


class DataMunging1 extends ShouldMatchers {
	val path = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat"
	val path2 = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat"

	@Test def shouldReadWeatherDataFile() {
		readFile(path).size should equalTo(40)
		readFile(path2).size should equalTo(27)
	}

	@Test def extractDataReturnsLinesContainingOnlyNumericalData() {
		val data = readFile(path)
		Assert.assertThat(extractData(data)(0), startsWith("   1"))
		Assert.assertThat(extractData(data)(29), startsWith("  30"))

		val data2 = readFile(path2)
		Assert.assertThat(extractData(data2)(0), startsWith("    1."))
		Assert.assertThat(extractData(data2)(19), startsWith("   20."))
	}

	@Test def shouldExtractFirstThreeColumns() {
		val lines = extractData(readFile(path))
		extractColumns(lines, 0, 1, 2)(0) should equalTo(Seq("1", "88", "59"))
		extractColumns(lines, 0, 1, 2)(25) should equalTo(Seq("26", "97*", "64"))
		extractColumns(lines, 0, 1, 2)(29) should equalTo(Seq("30", "90", "45"))

		val lines2 = extractData(readFile(path2))
		extractColumns(lines2, 1, 6, 8)(0) should equalTo(Seq("Arsenal", "79", "36"))
		extractColumns(lines2, 1, 6, 8)(19) should equalTo(Seq("Leicester", "30", "64"))
	}

	abstract class ARow[+T](val id: T, val min: Int, val max: Int)
	case class WeatherRow(day: Int, override val min: Int, override val max: Int) extends ARow[Int](day, min, max)
	case class FootballRow(team: String, override val min: Int, override val max: Int) extends ARow[String](team, min, max)

	@Test def shouldConvertDataIntoRowObjects() {
		val rows = extractColumns(extractData(readFile(path)), 0, 1, 2)
		convertIntoWeatherRows(rows)(0) should equalTo(WeatherRow(1, 88, 59))
		convertIntoWeatherRows(rows)(25) should equalTo(WeatherRow(26, 97, 64))

		val rows2 = extractColumns(extractData(readFile(path2)), 1, 6, 8)
		convertIntoFootballRows(rows2)(0) should equalTo(FootballRow("Arsenal", 79, 36))
	}

	@Test def shouldCalculateSpreads() {
		val rows = convertIntoWeatherRows(extractColumns(extractData(readFile(path)), 0, 1, 2))
		calcSpread(rows)(0)._2 should equalTo(29)
		calcSpread(rows)(29)._2 should equalTo(45)

		val rows2 = convertIntoFootballRows(extractColumns(extractData(readFile(path2)), 1, 6, 8))
		calcSpread(rows2)(0)._2 should equalTo(43)
		calcSpread(rows2)(19)._2 should equalTo(34)
	}

	@Test def shouldFindMinSpread() {
		val rows = calcSpread(convertIntoWeatherRows(extractColumns(extractData(readFile(path)), 0, 1, 2)))
		findMinSpread(rows).id should equalTo(14)

		val rows2 = calcSpread(convertIntoFootballRows(extractColumns(extractData(readFile(path2)), 1, 6, 8)))
		findMinSpread(rows2).id should equalTo("Aston_Villa")
	}

	def findMinSpread[T](rows: Seq[(T, Int)]) = rows.minBy(_._2)._1

	def calcSpread(seq: Seq[ARow[Any]]): Seq[(ARow[Any], Int)] = {
		seq.map{ row => (row, (row.max - row.min).abs ) }
	}

	def convertIntoNumber(rows: Seq[Seq[String]], columnIndexes: Int*) = rows.map{ row => row.map{_.replaceAll("[*.]", "").toInt }}
	def convertIntoWeatherRows(rows: Seq[Seq[String]], columnIndexes: Int*): Seq[ARow[Any]] = rows.map{ row =>
			val day = row(0).replaceAll("[*.]", "").toInt
			val min = row(1).replaceAll("[*.]", "").toInt
			val max = row(2).replaceAll("[*.]", "").toInt
			WeatherRow(day, min, max)
	}
	def convertIntoFootballRows(rows: Seq[Seq[String]], columnIndexes: Int*): Seq[ARow[Any]] = rows.map{ row =>
			val team = row(0)
			val min = row(1).replaceAll("[*.]", "").toInt
			val max = row(2).replaceAll("[*.]", "").toInt
			FootballRow(team, min, max)
	}

	def extractColumns(seq: Seq[String], columnIndexes: Int*): Seq[Seq[String]] = seq.map { row =>
		val columns = row.split("\\s+").drop(1)
		columnIndexes.map(columns)
	}

	def extractData(seq: Seq[String]): Seq[String] = seq.filter(startsWithNumber)

	def startsWithNumber(s: String) = s.matches(" *\\d+.*")

	def readFile(path:String) : Seq[String] = Source.fromFile(path).getLines().toSeq;
}