package ru.katas.n13

import org.scalatest.matchers.ShouldMatchers
import scala.io.Source
import org.junit.Test

/**
 * User: dima
 * Date: 15/03/2012
 */
class LineCount2 extends ShouldMatchers {
  def extractLinesWithCode(s: String): Seq[String] = {
    s.replaceAll("""(?s)/\*.*?\*/""", "")
      .replaceAll("//.*", "")
      .split("\n").filterNot { _.trim.isEmpty }
  }

  @Test def file3() {
    var s = Source.fromFile("/Users/dima/IdeaProjects/katas/src/main/java/ru/db/StringUtil.java").mkString
    val lines = extractLinesWithCode(s)

    println("Lines of code: " + lines.size)
    println(lines.mkString("\n"))

    lines.size should equal(10)
  }

  @Test def file2() {
    var s = Source.fromFile("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n13/java_source2.txt").mkString
    val lines = extractLinesWithCode(s)

    println("Lines of code: " + lines.size)
    println(lines.mkString("\n"))

    lines.size should equal(5)
  }

  @Test def file1() {
    var s = Source.fromFile("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n13/java_source1.txt").mkString
    val lines = extractLinesWithCode(s)

    println("Lines of code: " + lines.size)
    println(lines.mkString("\n"))

    lines.size should equal(3)
  }
}