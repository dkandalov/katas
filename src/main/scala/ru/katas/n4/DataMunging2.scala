package ru.katas.n4

import ru.util.Arcade
import scala.io.Source
import org.scalatest.FunSuite

/**
 * User: dima
 * Date: 24/06/2012
 */

@Arcade
class DataMunging2 extends FunSuite {
	test("should find rows with min temperature spread and min different between for and against goals") {
		assert(findMinRow("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 1, 2, 3) == ("14", 2))
		assert(findMinRow("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 2, 7, 9) == ("Aston_Villa", 1))
	}

	def findMinRow(fileName: String, idCol: Int, minCol: Int, maxCol: Int): (String, Int) = {
		Source.fromFile(fileName).getLines().toSeq.filter(_.matches(" +\\d+.*"))
			.map(_.replace("*", "").split(" +")).map { row => (row(idCol), (row(minCol).toInt - row(maxCol).toInt).abs)}.minBy(_._2)
	}
}