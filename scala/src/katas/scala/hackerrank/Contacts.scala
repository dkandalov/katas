package katas.scala.hackerrank

import katas.scala.Util
import org.junit.{Ignore, Test}
import org.scalatest.Matchers

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Random

/**
	* https://www.hackerrank.com/challenges/contacts
	*/
class Contacts extends Matchers {

	@Test def `add and find contacts`(): Unit = {
		val contacts = new Contacts()
		contacts.add("value")
		contacts.add("value2")
		contacts.amountOfMatches("val") shouldEqual 2
		contacts.amountOfMatches("value") shouldEqual 2
		contacts.amountOfMatches("value2") shouldEqual 1
		contacts.amountOfMatches("value3") shouldEqual 0
	}

	@Test def `hackerrank test`(): Unit = {
		val contacts = new Contacts()
		contacts.add("hack")
		contacts.add("hackerrank")
		contacts.amountOfMatches("hac") shouldEqual 2
		contacts.amountOfMatches("hak") shouldEqual 0
	}

	@Ignore
	@Test def `adding maximum amount of elements`(): Unit = {
		val contacts = new Contacts()
		val random = new Random(123)
		0.until(100000).foreach { _ =>
			contacts.add(random.nextString(21))
		}
	}

	@Test def `hackerrank test case 2 (fixing performance)`(): Unit = {
		val lines = Source.fromFile("scala/src/katas/scala/hackerrank/Contacts-testcase2-input.txt").getLines()
		main(lines.toStream)
	}

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)
		main(lines = Stream.continually(scanner.nextLine()))
	}

	def main(lines: Stream[String]): Unit = {
		val n = lines.head.toInt
		val i = lines.tail.iterator

		var addDuration = Duration.Zero
		var matchDuration = Duration.Zero

		val contacts = new Contacts()
		0.until(n).foreach { _ =>
			val parts = i.next().split(" +")
			val command = parts(0)
			val value = parts(1)
			if (command == "add") {
				addDuration += Util.measureDuration {
					contacts.add(value)
				}
			} else if (command == "find") {
				matchDuration += Util.measureDuration {
					println(contacts.amountOfMatches(value))
				}
			}
		}
		println(addDuration)
		println(matchDuration)
	}

	private case class Node(
     value: Char = '\0',
     children: mutable.Map[Char, Node] = mutable.Map()
  ) {

		def add(s: String): Unit = {
			if (s.isEmpty) return
			var childNode = children.getOrElse(s.head, null)
			if (childNode == null) {
				childNode = Node(s.head)
				children.put(childNode.value, childNode)
			}
			childNode.add(s.tail)
		}

		def deepestMatchingNode(s: String): Node = {
			if (s.isEmpty) return this
			val childNode = children.getOrElse(s.head, null)
			if (childNode == null) null else childNode.deepestMatchingNode(s.tail)
		}

		def leafCount: Int = {
			if (children.isEmpty) 1
			else children.values.foldLeft(0){ (sum, node) => sum + node.leafCount }
		}

		override def toString: String = value.toString
	}

	private class Contacts {
		private val tree = Node()

		def add(s: String): Unit = tree.add(s + '\0')

		def amountOfMatches(s: String): Int = {
			val node = tree.deepestMatchingNode(s)
			if (node == null || node == tree) 0 else node.leafCount
		}
	}
}