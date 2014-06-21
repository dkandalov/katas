package ru.bsearchtree

import org.scalatest.Matchers
import org.junit.Test


class _234BST extends Matchers {
	@Test def `inserting elements`() {
		Empty().insert("a") should equal(Node2("a"))
		Empty().insert("a", "s") should equal(Node3(("a", "s")))
		Empty().insert("a", "s", "e") should equal(Node4(("a", "e", "s")))
		Empty().insert("a", "s", "e", "r") should equal(
			Node2("e", (
				Node2("a"),
				Node3(("r", "s"))
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
			if (newValue > value)
				Node3((value, newValue), (children._1, children._2, Empty()))
			else
				Node3((newValue, value), (children._1, children._2, Empty()))
		}
	}

	private case class Node3(value: (String, String),
	                         children: (Tree, Tree, Tree) = (Empty(), Empty(), Empty())) extends Tree {
		override def insert(newValue: String) = {
			if (newValue > value._1 && newValue > value._2)
				Node4((value._1, value._2, newValue), (children._1, children._2, children._3, Empty()))
			else if (newValue > value._1 && newValue <= value._2) {
				Node4((value._1, newValue, value._2), (children._1, children._2, Empty(), children._3))
			} else {
				Node4((newValue, value._1, value._2), (children._1, Empty(), children._2, children._3))
			}
		}
	}
	private case class Node4(value: (String, String, String),
	                         children: (Tree, Tree, Tree, Tree) = (Empty(), Empty(), Empty(), Empty())) extends Tree {
		override def insert(newValue: String) = {
			split.insert(newValue)
		}

		private def split: Node2 = Node2(value._2, (Node2(value._1), Node2(value._3)))
	}
}