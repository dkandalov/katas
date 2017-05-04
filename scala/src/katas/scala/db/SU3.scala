package katas.scala.db

import java.lang.{Character => JCharacter, Integer => JInteger}
import java.util

import org.junit.Test
import org.specs2.matcher.{BeEqualTo, ShouldMatchers}

import scala.collection.JavaConverters._

/**
 * User: dima
 * Date: 09/04/2012
 */

class SU3 extends StringUtil {
  override def sort(input: String): String = input.sorted

  override def reverse(input: String): String = input.reverse

  override def getDistribution(input: String): util.HashMap[JCharacter, JInteger] = {
    input.foldLeft(new util.HashMap[JCharacter, JInteger]()) { (acc, c) =>
      var count = acc.get(c)
      if (count == null) count = 0
      acc.put(c, count + 1)
      acc
    }
  }

  override def getFirstNSortedChars(input: String, topN: Int): String = input.sorted.substring(0, topN)

  override def getUniqueCharsSortedByOccurrence(input: String): String = {
    revertedDistributionOf(input).foldLeft("") { _ + _._2 }
  }

  override def getMode(input: String): String = {
    val charsByFreq = revertedDistributionOf(input)
    charsByFreq match {
      case List() => ""
      case _ =>
        val freqOfFirst = charsByFreq.head._1
        charsByFreq.takeWhile{ _._1 == freqOfFirst }.foldLeft("") { _ + _._2 }
    }
  }

  private def revertedDistributionOf(input: String) : List[(JInteger, JCharacter)] = {
    getDistribution(input)
      .entrySet().asScala
      .foldLeft(List[(JInteger, JCharacter)]()) { (acc: List[(JInteger, JCharacter)], e: util.Map.Entry[JCharacter, JInteger]) =>
        (e.getValue, e.getKey) :: acc
      }
      .sortBy(-_._1)
  }
}


class SU3Test extends ShouldMatchers {
  private val utils = new SU3()

  @Test def sort() {
    utils.sort("") should equalTo("")
    utils.sort("a") should equalTo("a")
    utils.sort("ba") should equalTo("ab")
    utils.sort("bca") should equalTo("abc")
  }

  @Test def reverse() {
    utils.reverse("") should equalTo("")
    utils.reverse("abc") should equalTo("cba")
  }

  @Test def distribution() {
    utils.getDistribution("") should equalTo(new util.HashMap())
    utils.getDistribution("aabbbc") should equalScalaMap(Map('a' -> 2, 'b' -> 3, 'c' -> 1))
  }

  @Test def firstNSortedChars() {
    utils.getFirstNSortedChars("", 0) should equalTo("")
    utils.getFirstNSortedChars("a", 0) should equalTo("")
    utils.getFirstNSortedChars("a", 1) should equalTo("a")
    utils.getFirstNSortedChars("bcba", 2) should equalTo("ab")
  }

  @Test def uniqueCharsSortedByOccurence() {
    utils.getUniqueCharsSortedByOccurrence("") should equalTo("")
    utils.getUniqueCharsSortedByOccurrence("a") should equalTo("a")
    utils.getUniqueCharsSortedByOccurrence("abb") should equalTo("ba")
  }

  @Test def mode() {
    utils.getMode("") should equalTo("")
    utils.getMode("a") should equalTo("a")
    utils.getMode("abb") should equalTo("b")
    utils.getMode("abab") should equalTo("ab")
  }

  @Test def equalScalaMap() {
    // "" should equalScalaMap(Map()) // should fail

    new util.HashMap() should equalScalaMap(Map())

    val map = new util.HashMap[JCharacter, JInteger]()
    map.put('a', 1)
//    map should equalScalaMap(Map()) // should fail
    map should equalScalaMap(Map('a' -> 1))
//    map should equalScalaMap(Map('b' -> 1)) // should fail
  }

  def equalScalaMap(right: Map[Char, Int]): BeEqualTo = {
    val rightMap = right.foldLeft(new util.HashMap[JCharacter, JInteger]) { (acc, e) =>
      acc.put(e._1, e._2)
      acc
    }
    equalTo(rightMap)
  }
}


