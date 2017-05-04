package katas.scala.classic_katas.bloomfilter

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

/**
 * User: dima
 * Date: 12/09/2012
 */

class Bloom0 extends ShouldMatchers {
	@Test def determineIfWordIsInFilter_InSimplestCases() {
		val bloomFilter = new BloomFilter(100)

		bloomFilter.contains("aaa") should equalTo(false)
		bloomFilter.contains("bbb") should equalTo(false)
		bloomFilter.add("aaa")
		bloomFilter.contains("aaa") should equalTo(true)
		bloomFilter.contains("bbb") should equalTo(false)

		bloomFilter.data.count{ _ == true } should equalTo(3)
	}

	@Test def determineIfWordIsInFilter_ForRealDictionary() {
		val bloomFilter = new BloomFilter(2000000)

		val words = Source.fromFile("/usr/share/dict/words").getLines().toSeq

		words.foreach{ bloomFilter.add(_) }
		bloomFilter.data.count{ _ == true } should equalTo(592227)
		bloomFilter.contains("aaa") should equalTo(false)
		bloomFilter.contains("bbb") should equalTo(false)

		var count = 0
		for (i <- 3 to 6; j <- 1 to 20) {
			val randomWord = Random.alphanumeric.take(i).mkString
			if (bloomFilter.contains(randomWord)) {
				println(randomWord)
				count += 1
			}
		}
		println("count = " + count)
		count < 10 should equalTo(true)
	}

	private class BloomFilter(size: Int) {
		val data: ArrayBuffer[Boolean] = ArrayBuffer.fill(size) {false}

		def add(word: String) {
			data(hash1Of(word)) = true
			data(hash2Of(word)) = true
			data(hash3Of(word)) = true
		}

		def contains(word: String): Boolean = {
			data(hash1Of(word)) && data(hash2Of(word)) && data(hash3Of(word))
		}

		private def hash1Of(s: String): Int = {
			s.foldLeft(17) { (acc, c) => 31 * acc + c }.abs % size
		}

		private def hash2Of(s: String): Int = {
			s.foldLeft(7) { (acc, c) => 11 * acc + c }.abs % size
		}

		private def hash3Of(s: String): Int = {
			s.foldLeft(5) { (acc, c) => 7 * acc + c }.abs % size
		}
	}
}