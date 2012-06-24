package ru.katas.n4

import ru.util.Arcade
import org.junit.Test
import scala.io.Source
import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 24/06/2012
 */

@Arcade
class DataMunging2 extends ShouldMatchers {
	@Test def aaa() {
		findMinRow("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 1, 2, 3) should equal("14", 2)
		findMinRow("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 2, 7, 9) should equal("Aston_Villa", 1)
	}

	def findMinRow(fileName: String, idCol: Int, minCol: Int, maxCol: Int): (String, Int) = {
		Source.fromFile(fileName).getLines().toSeq.filter(_.matches(" +\\d+.*"))
			.map(_.replace("*", "").split(" +")).map { row => (row(idCol), (row(minCol).toInt - row(maxCol).toInt).abs)}.minBy(_._2)
	}
}