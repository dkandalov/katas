package ru.katas.n4

import org.scalatest.matchers.ShouldMatchers
import scala.io.Source
import org.hamcrest.Matchers
import org.junit.{Assert, Test}
import collection.Seq


class DataMunging1 extends ShouldMatchers {
	val path = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat"
	val path2 = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat"

	@Test def shouldReadWeatherDataFile() {
		readFile(path).size should equal(40)
		readFile(path2).size should equal(27)
	}

	@Test def extractDataReturnsLinesContainingOnlyNumericalData() {
		val data = readFile(path)
		Assert.assertThat(extractData(data)(0), Matchers.startsWith("   1"))
		Assert.assertThat(extractData(data)(29), Matchers.startsWith("  30"))

		val data2 = readFile(path2)
		Assert.assertThat(extractData(data2)(0), Matchers.startsWith("    1."))
		Assert.assertThat(extractData(data2)(19), Matchers.startsWith("   20."))
	}

	@Test def shouldExtractFirstThreeColumns() {
		val lines = extractData(readFile(path))
		extractColumns(lines, 0, 1, 2)(0) should equal(Seq("1", "88", "59"))
		extractColumns(lines, 0, 1, 2)(25) should equal(Seq("26", "97*", "64"))
		extractColumns(lines, 0, 1, 2)(29) should equal(Seq("30", "90", "45"))

		val lines2 = extractData(readFile(path2))
		extractColumns(lines2, 0, 6, 8)(0) should equal(Seq("1.", "79", "36"))
		extractColumns(lines2, 0, 6, 8)(19) should equal(Seq("20.", "30", "64"))
	}

	@Test def shouldConvertIntoNumericData() {
		val rows = extractColumns(extractData(readFile(path)), 0, 1, 2)
		convertIntoNumber(rows)(0) should equal(Seq(1, 88, 59))
		convertIntoNumber(rows)(25) should equal(Seq(26, 97, 64))

		val rows2 = extractColumns(extractData(readFile(path2)), 0, 6, 8)
		convertIntoNumber(rows2)(0) should equal(Seq(1, 79, 36))
	}

	@Test def shouldCalculateSpreads() {
		val rows = convertIntoNumber(extractColumns(extractData(readFile(path)), 0, 1, 2))
		calcSpread(rows)(0) should equal((1, 29))
		calcSpread(rows)(29) should equal((30, 45))

		val rows2 = convertIntoNumber(extractColumns(extractData(readFile(path2)), 0, 6, 8))
		calcSpread(rows2)(0) should equal((1, 43))
		calcSpread(rows2)(19) should equal((20, 34))
	}

	@Test def shouldFindMinSpread() {
		val rows = calcSpread(convertIntoNumber(extractColumns(extractData(readFile(path)), 0, 1, 2)))
		findMinSpread(rows) should equal(14)
	}

	def findMinSpread(rows: Seq[(Int, Int)]) = rows.minBy(_._2)._1

	def calcSpread(seq: Seq[Seq[Int]]): Seq[(Int, Int)] = {
		seq.map{ row => (row(0), (row(1) - row(2)).abs ) }
	}

	def convertIntoNumber(rows: Seq[Seq[String]]) = rows.map{ row => row.map{_.replaceAll("[*.]", "").toInt }}

	def extractColumns(seq: Seq[String], columnIndexes: Int*): Seq[Seq[String]] = seq.map { row =>
		val columns = row.split("\\s+").drop(1)
		columnIndexes.map(columns)
	}

	def extractData(seq: Seq[String]): Seq[String] = seq.filter(startsWithNumber)

	def startsWithNumber(s: String) = s.matches(" *\\d+.*")

	def readFile(path:String) : Seq[String] = Source.fromFile(path).getLines().toSeq;
}