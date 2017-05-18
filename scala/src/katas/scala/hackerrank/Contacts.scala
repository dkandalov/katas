package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable

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

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)
		val n = scanner.nextLine().toInt
		val contacts = new Contacts()

		0.until(n).foreach { _ =>
			val parts = scanner.nextLine().split(" +")
			val command = parts(0)
			val value = parts(1)
			if (command == "add") {
				contacts.add(value)
			} else if (command == "find") {
				val amountOfMatches: Int = contacts.amountOfMatches(value)
				println(amountOfMatches)
			}
		}
	}

	private case class Node(value: Char = '\0', children: mutable.Set[Node] = mutable.Set()) {

		def add(s: String): Unit = {
			if (s.isEmpty) return
			var childNode = children.find(_.value == s.head).orNull
			if (childNode == null) {
				childNode = Node(s.head)
				children.add(childNode)
			}
			childNode.add(s.tail)
		}

		def deepestMatchingNode(s: String): Node = {
			if (s.isEmpty) return this
			val childNode = children.find(_.value == s.head).orNull
			if (childNode == null) null else childNode.deepestMatchingNode(s.tail)
		}

		def leafCount: Int = {
			if (children.isEmpty) 1
			else children.toList.map{ _.leafCount }.sum
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