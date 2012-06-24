package ru.katas.n4

import ru.util.Arcade
import org.junit.Test
import scala.io.Source

/**
 * User: dima
 * Date: 24/06/2012
 */

@Arcade
class DataMunging2 {
	@Test def aaa() {
		println(
			findMinRow("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 1, 2, 3)
		)
		println(
			Source.fromFile("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").getLines().toSeq.filter(_.matches(" +\\d+.*"))
				.map(_.replace("*", "").split(" +")).map{row => (row(2), (row(7).toInt - row(9).toInt).abs)}.minBy(_._2)
		)
	}

	def findMinRow(fileName: String, idCol: Int, minCol: Int, maxCol: Int): (String, Int) = {
		Source.fromFile(fileName).getLines().toSeq.filter(_.matches(" +\\d+.*"))
			.map(_.replace("*", "").split(" +")).map {
			row => (row(idCol), (row(minCol).toInt - row(maxCol).toInt).abs)
		}.minBy(_._2)
	}
}