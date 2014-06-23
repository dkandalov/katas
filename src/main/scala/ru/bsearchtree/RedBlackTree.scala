package ru.bsearchtree

import org.scalatest.Matchers
import org.junit.Test


class RedBlackTree extends Matchers {
	@Test def `inserting elements as described in 'Algorithms in Java' Figure 13.13`() {
		Empty().insert("a") should equal(BlackNode("a"))
		Empty().insert("a", "s") should equal(BlackNode("a", Empty(), RedNode("s")))
	}

	private abstract class Tree {
		def insert(newValue: String): Tree
		def insert(newValues: String*): Tree = {
			if (newValues.isEmpty) this
			else insert(newValues.head).insert(newValues.tail : _*)
		}
	}

	private case class Empty() extends Tree {
		override def insert(newValue: String) = BlackNode(newValue)
	}

	private case class BlackNode(value: String, left: Tree = Empty(), right: Tree = Empty()) extends Tree {
		override def insert(newValue: String) = ???
	}

	private case class RedNode(value: String) extends Tree {
		override def insert(newValue: String) = ???
	}
}