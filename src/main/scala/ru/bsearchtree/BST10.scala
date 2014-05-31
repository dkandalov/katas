package ru.bsearchtree

import org.junit.Test
import org.scalatest.Matchers


class BST10 extends Matchers {
	@Test def `size of tree`() {
		EmptyNode().size() should equal(0)
		Node("B").size() should equal(1)
		Node("B", Node("A"), Node("C")).size() should equal(3)
	}

	@Test def `node insertion`() {
		EmptyNode().insert("A") should equal(Node("A"))
		Node("B").insert("A") should equal(Node("B", Node("A"), EmptyNode()))
		Node("B").insert("A").insert("C") should equal(Node("B", Node("A"), Node("C")))
	}

	@Test def `root node insertion`() {
		EmptyNode().rootInsert("A") should equal(Node("A"))
		Node("B").rootInsert("A") should equal(Node("A", EmptyNode(), Node("B")))
		Node("B").rootInsert("A").rootInsert("C") should equal(Node("C", Node("A", EmptyNode(), Node("B"))))
	}

	private abstract class Tree(value: String, left: Tree = EmptyNode(), right: Tree = EmptyNode()) {
		def size(): Int
		def insert(value: String): Tree
		def rootInsert(value: String): Tree
	}
	private case class Node(value: String, left: Tree = EmptyNode(), right: Tree = EmptyNode()) extends Tree(value, left, right) {
		def size(): Int = {
			1 + left.size() + right.size
		}

		override def insert(value: String) = {
			if (value < this.value) Node(this.value, left.insert(value), right)
			else Node(this.value, left, right.insert(value))
		}

		override def rootInsert(value: String) = {
			if (value < this.value) {
				Node(this.value, left.rootInsert(value), right).rotateRight()
			} else {
				Node(this.value, left, right.rootInsert(value)).rotateLeft()
			}
		}

		private def rotateRight(): Node = left match {
			case EmptyNode() => this
			case leftNode: Node =>
				Node(leftNode.value, leftNode.left, Node(value, leftNode.right, right))
		}
		private def rotateLeft(): Node = right match {
			case EmptyNode() => this
			case rightNode: Node =>
				Node(rightNode.value, Node(value, left, rightNode.left), rightNode.right)
		}
	}
	private case class EmptyNode() extends Tree(null, null, null) {
		override def size() = 0
		override def insert(value: String) = Node(value)
		override def rootInsert(value: String) = Node(value)
	}
}