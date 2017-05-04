package katas.scala.classic_katas.datamunging

import org.specs2.matcher.ShouldMatchers
import org.junit.Test
import scala.io.Source

/**
 * User: dima
 * Date: 15/03/2012
 */
class DataMunging0 extends ShouldMatchers {
  @Test def findDayWithMinTemperatureSpread() {
    val lines = trimFile("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 8, 2)
    lines.foreach{ line => println(line) }

    val rows = lines.map{ line =>
        val row = line.split("\\s+")
        val lineNumber = row(1).toInt
        val maxTemperature = row(2).replaceAll("\\*", "").toInt
        val minTemperature = row(3).replaceAll("\\*", "").toInt

        (lineNumber,  MaxMin(maxTemperature, minTemperature))
    }
//    rows.foreach{row( Row => println(row)}

    val rowWithMinSpread = rows.minBy { row => math.abs(row._2.max - row._2.min) }
    println(rowWithMinSpread)

  }

  case class MaxMin(max: Int, min: Int)

//  def smallestDiff(name: String,  m: MaxMin)

  @Test def findATeamWithSmallestDifference() {
    var lines = trimFile("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 5, 1)
    lines = lines.filterNot{ _.matches(" *[-]+") }

    val scores = lines.map{ line =>
      val row = line.split("\\s+")
      (row(2), new MaxMin(row(7).toInt, row(9).toInt))
    }
    println(scores.minBy { x => (x._2.max - x._2.min).abs}   )
  }

  def trimFile(file: String, top: Int, bottom: Int): List[String] = {
    val source = Source.fromFile(file)
    var lines = source.getLines().toList
    lines.take(lines.length - bottom).drop(top)
  }
}