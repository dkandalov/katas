package ru.bsearchtree

import org.junit.Test
import org.scalatest.Matchers


class BST10 extends Matchers {
	@Test def `size of tree`() {
		EmptyNode().size() should equal(0)
		Node("b").size() should equal(1)
		Node("b", Node("a"), Node("c")).size() should equal(3)
	}

	@Test def `node insertion`() {
		EmptyNode().insert("a") should equal(Node("a"))
		Node("b").insert("a") should equal(Node("b", Node("a"), EmptyNode()))
		Node("b").insert("a").insert("c") should equal(Node("b", Node("a"), Node("c")))
	}

	@Test def `root node insertion`() {
		EmptyNode().rootInsert("a") should equal(Node("a"))
		Node("b").rootInsert("a") should equal(Node("a", EmptyNode(), Node("b")))
		Node("b").rootInsert("a").rootInsert("c") should equal(Node("c", Node("a", EmptyNode(), Node("b"))))

		val tree = "aserchi".foldLeft(EmptyNode().asInstanceOf[Tree]) { (tree, c) => tree.rootInsert(c.toString) }
		tree should equal(
			Node("i",
				Node("h",
					Node("c",
						Node("a"),
						Node("e"))),
				Node("r",
					EmptyNode(),
					Node("s"))
		))
	}

	@Test def `splay root node insertion`() {
		EmptyNode().splayInsert("a") should equal(Node("a"))
		Node("b").splayInsert("a") should equal(Node("a", EmptyNode(), Node("b")))
		Node("b").splayInsert("a").splayInsert("c") should equal(Node("c", Node("b", Node("a"))))

		val tree = "aserching".foldLeft(EmptyNode().asInstanceOf[Tree]) { (tree, c) => tree.splayInsert(c.toString) }
		tree should equal(
			Node("g",
				Node("e",
					Node("c",
						Node("a"))),
				Node("i",
					Node("h"),
					Node("n",
						EmptyNode(),
						Node("r",
							EmptyNode(),
							Node("s")))
			)))
	}


	private abstract class Tree(value: String, left: Tree = EmptyNode(), right: Tree = EmptyNode()) {
		def size(): Int
		def insert(value: String): Tree
		def rootInsert(value: String): Tree
		def splayInsert(value: String): Tree
		def rotateRight(): Tree
		def rotateLeft(): Tree
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

		override def splayInsert(value: String) = {
			if (value < this.value) {
				left match {
					case EmptyNode() =>
						Node(this.value, Node(value), this.right).rotateRight()

					case Node(leftValue, leftLeft, leftRight) =>
						if (value < leftValue)
							Node(this.value, Node(leftValue, leftLeft.splayInsert(value), leftRight), right)
								.rotateRight().rotateRight()
						else
							Node(this.value, Node(leftValue, leftLeft, leftRight.splayInsert(value)).rotateLeft(), right)
								.rotateRight()
				}
			} else {
				right match {
					case EmptyNode() =>
						Node(this.value, left, Node(value)).rotateLeft()

					case Node(rightValue, rightLeft, rightRight) =>
						if (value > rightValue)
							Node(this.value, left, Node(rightValue, rightLeft, rightRight.splayInsert(value)))
								.rotateLeft().rotateLeft()
						else
							Node(this.value, left, Node(rightValue, rightLeft.splayInsert(value), rightRight).rotateRight())
								.rotateLeft()
				}
			}
		}

		override def rotateRight(): Node = left match {
			case EmptyNode() => this
			case leftNode: Node =>
				Node(leftNode.value, leftNode.left, Node(value, leftNode.right, right))
		}

		override def rotateLeft(): Node = right match {
			case EmptyNode() => this
			case rightNode: Node =>
				Node(rightNode.value, Node(value, left, rightNode.left), rightNode.right)
		}

		override def toString = (left, right) match {
			case (EmptyNode(), EmptyNode()) => "(" + value + ")"
			case _ => "(" + value + ", " + left + ", " + right + ")"
		}
	}

	private case class EmptyNode() extends Tree(null, null, null) {
		override def size() = 0
		override def insert(value: String) = Node(value)
		override def rootInsert(value: String) = Node(value)
		override def splayInsert(value: String) = Node(value)
		override def rotateLeft() = this
		override def rotateRight() = this
		override def toString = "-"
	}
}