package ru.bsearchtree

import org.scalatest.Matchers
import org.junit.Test


class _234BST extends Matchers {
	@Test def `inserting elements`() {

	}

	private abstract class Tree {
		def insert(value: String): Tree
	}
	private case class Empty() extends Tree {
		override def insert(value: String) = Node2(value)
	}
	private case class Node2(value: String,
	                         left: Tree = Empty(), right: Tree = Empty()) extends Tree {
		override def insert(value: String) = {
			this
		}
	}
	private case class Node3(value: (String, String),
	                         left: Tree = Empty(), mid: Tree = Empty(), right: Tree = Empty()) extends Tree {
		override def insert(value: String) = ???
	}
	private case class Node4(value: (String, String, String),
	                         left: Tree = Empty(), mid1: Tree = Empty(), mid2: Tree = Empty(), right: Tree = Empty()) extends Tree {
		override def insert(value: String) = ???
	}
}