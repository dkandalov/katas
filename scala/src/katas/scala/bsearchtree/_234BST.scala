package katas.scala.bsearchtree

import org.junit.Test
import org.scalatest.Matchers


class _234BST extends Matchers {
	@Test def `inserting elements as described in 'Algorithms in Java' Figure 13.13`() {
		Empty().insert("a") should equal(Node2("a"))
		Empty().insert("a", "s") should equal(Node3(("a", "s")))
		Empty().insert("a", "s", "e") should equal(Node4(("a", "e", "s")))
		Empty().insert("a", "s", "e", "r") should equal(
			Node2("e", (
				Node2("a"),
				Node3(("r", "s"))
			))
		)
		Empty().insert("a", "s", "e", "r", "c") should equal(
			Node2("e", (
				Node3(("a", "c")),
				Node3(("r", "s"))
			))
		)
		Empty().insert("a", "s", "e", "r", "c", "h") should equal(
			Node2("e", (
				Node3(("a", "c")),
				Node4(("h", "r", "s"))
			))
		)
		Empty().insert("a", "s", "e", "r", "c", "h", "i") should equal(
			Node3(("e", "r"), (
				Node3(("a", "c")),
				Node3(("h", "i")),
				Node2("s")
			))
		)
		Empty().insert("a", "s", "e", "r", "c", "h", "i", "n") should equal(
			Node3(("e", "r"), (
				Node3(("a", "c")),
				Node4(("h", "i", "n")),
				Node2("s")
			))
		)
		Empty().insert("a", "s", "e", "r", "c", "h", "i", "n", "g") should equal(
			Node4(("e", "i", "r"), (
				Node3(("a", "c")),
				Node3(("g", "h")),
				Node2("n"),
				Node2("s")
			))
		)
		Empty().insert("a", "s", "e", "r", "c", "h", "i", "n", "g", "x") should equal(
			Node2("i", (
				Node2("e", (
					Node3(("a", "c")),
					Node3(("g", "h"))
				)),
				Node2("r",(
					Node2("n"),
					Node3(("s", "x"))
				))
			))
		)
	}

	private abstract class Tree {
		def insert(newValue: String): Tree
		def insert(newValues: String*): Tree = {
			if (newValues.isEmpty) this
			else insert(newValues.head).insert(newValues.tail.toArray : _*)
		}
	}

	private case class Empty() extends Tree {
		override def insert(newValue: String) = Node2(newValue)
	}

	private case class Node2(value: String,
	                         children: (Tree, Tree) = (Empty(), Empty())) extends Tree {
		override def insert(newValue: String) = {
			if (newValue > value) {
				if (children._2.isInstanceOf[Node4]) {
					val node4 = children._2.asInstanceOf[Node4]
					Node3((value, node4.value._2), (
						children._1,
						Node2(node4.value._1, (node4.children._1, node4.children._2)),
						Node2(node4.value._3, (node4.children._3, node4.children._4))
					)).insert(newValue)

				} else if (children._2 != Empty()) {
					Node2(value, (children._1, children._2.insert(newValue)))
				} else {
					Node3((value, newValue), (children._1, children._2, Empty()))
				}

			} else {
				if (children._1.isInstanceOf[Node4]) {
					val node4 = children._1.asInstanceOf[Node4]
					Node3((node4.value._2, value), (
						Node2(node4.value._1, (node4.children._1, node4.children._2)),
						Node2(node4.value._3, (node4.children._3, node4.children._4)),
						children._2
					)).insert(newValue)

				} else if (children._1 != Empty()) {
					Node2(value, (children._1.insert(newValue), children._2))
				} else {
					Node3((newValue, value), (children._1, children._2, Empty()))
				}
			}
		}
	}

	private case class Node3(value: (String, String),
	                         children: (Tree, Tree, Tree) = (Empty(), Empty(), Empty())) extends Tree {
		override def insert(newValue: String) = {
			if (newValue > value._1 && newValue > value._2)
				if (children._3.isInstanceOf[Node4]) {
					val node4 = children._3.asInstanceOf[Node4]
					Node4((value._1, value._2, node4.value._2), (
						children._1,
						children._2,
						Node2(node4.value._1, (node4.children._1, node4.children._2)),
						Node2(node4.value._3, (node4.children._3, node4.children._4))
					)).insertWithoutSplit(newValue)
				} else if (children._3 != Empty()) {
					Node3(value, (children._1, children._2, children._3.insert(newValue)))
				} else {
					Node4((value._1, value._2, newValue), (children._1, children._2, children._3, Empty()))
				}

			else if (newValue > value._1 && newValue <= value._2) {
				if (children._2.isInstanceOf[Node4]) {
					val node4 = children._2.asInstanceOf[Node4]
					Node4((value._1, node4.value._2, value._2), (
						children._1,
						Node2(node4.value._1, (node4.children._1, node4.children._2)),
						Node2(node4.value._3, (node4.children._3, node4.children._4)),
						children._3
					)).insertWithoutSplit(newValue)
				} else if (children._2 != Empty()) {
					Node3(value, (children._1, children._2.insert(newValue), children._3))
				} else {
					Node4((value._1, newValue, value._2), (children._1, children._2, Empty(), children._3))
				}

			} else {
				if (children._1.isInstanceOf[Node4]) {
					val node4 = children._1.asInstanceOf[Node4]
					Node4((node4.value._2, value._1, value._2), (
						Node2(node4.value._1, (node4.children._1, node4.children._2)),
						Node2(node4.value._3, (node4.children._3, node4.children._4)),
						children._2,
						children._3
					)).insertWithoutSplit(newValue)
				} else if (children._1 != Empty()) {
					Node3(value, (children._1.insert(newValue), children._2, children._3))
				} else {
					Node4((newValue, value._1, value._2), (children._1, Empty(), children._2, children._3))
				}
			}
		}
	}

	private case class Node4(value: (String, String, String),
	                         children: (Tree, Tree, Tree, Tree) = (Empty(), Empty(), Empty(), Empty())) extends Tree {
		override def insert(newValue: String) = {
			split.insert(newValue)
		}

		def insertWithoutSplit(newValue: String): Tree = {
			if (newValue > value._1 && newValue > value._2 && newValue > value._3) {
				Node4(value, (children._1, children._2, children._3, children._4.insert(newValue)))

			} else if (newValue > value._1 && newValue > value._2 && newValue <= value._3) {
				Node4(value, (children._1, children._2, children._3.insert(newValue), children._4))

			} else if (newValue > value._1 && newValue <= value._2 && newValue <= value._3) {
				Node4(value, (children._1, children._2.insert(newValue), children._3, children._4))

			} else {
				Node4(value, (children._1.insert(newValue), children._2, children._3, children._4))
			}
		}

		private def split: Node2 = {
			Node2(value._2, (
				Node2(value._1, (children._1, children._2)),
				Node2(value._3, (children._3, children._4))
			))
		}
	}
}