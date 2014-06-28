package ru.bsearchtree

import org.scalatest.Matchers
import org.junit.Test


class RedBlackTree extends Matchers {
	private object NodeColor extends Enumeration {
		type NodeColor = Value
		val None, Red, Black = Value
	}
	import NodeColor._


	@Test def `inserting elements as described in 'Algorithms in Java' Figure 13.20`() {
		emptyNode().insert("a") should equal(Node("a", Black))
		emptyNode().insert("a", "s") should equal(Node("a", Black, emptyNode(), Node("s", Red)))
		emptyNode().insert("a", "s", "e") should equal(Node("e", Black, Node("a", Red), Node("s", Red)))
		emptyNode().insert("a", "s", "e", "r") should equal(
			Node("e", Black,
				Node("a", Black),
				Node("s", Black, Node("r", Red))
		))
		emptyNode().insert("a", "s", "e", "r", "c") should equal(
			Node("e", Black,
				Node("a", Black, emptyNode(), Node("c", Red)),
				Node("s", Black, Node("r", Red))
		))
		emptyNode().insert("a", "s", "e", "r", "c", "h") should equal(
			Node("e", Black,
				Node("a", Black, emptyNode(), Node("c", Red)),
				Node("r", Black, Node("h", Red), Node("s", Red))
		))
		emptyNode().insert("a", "s", "e", "r", "c", "h", "i") should equal(
			Node("e", Black,
				Node("a", Black, emptyNode(), Node("c", Red)),
				Node("r", Red,
					Node("h", Black, emptyNode(), Node("i", Red)),
					Node("s", Black))
		))
		emptyNode().insert("a", "s", "e", "r", "c", "h", "i", "n") should equal(
			Node("e", Black,
				Node("a", Black, emptyNode(), Node("c", Red)),
				Node("r", Red,
					Node("i", Black, Node("h", Red), Node("n", Red)),
					Node("s", Black))
		))
		emptyNode().insert("a", "s", "e", "r", "c", "h", "i", "n", "g") should equal(
			Node("i", Black,
				Node("e", Red,
					Node("a", Black, emptyNode(), Node("c", Red)),
					Node("h", Black, Node("g", Red))),
				Node("r", Red,
					Node("n", Black),
					Node("s", Black))
		))
		emptyNode().insert("a", "s", "e", "r", "c", "h", "i", "n", "g", "x") should equal(
			Node("i", Black,
				Node("e", Black,
					Node("a", Black, emptyNode(), Node("c", Red)),
					Node("h", Black, Node("g", Red))),
				Node("r", Black,
					Node("n", Black),
					Node("s", Black, emptyNode(), Node("x", Red)))
		))
	}


	private def emptyNode(): Node = {
		Node(null, None, null, null)
	}

	private case class Node(value: String, var color: NodeColor = None, var left: Node = emptyNode(), var right: Node = emptyNode()) {
		def insert(newValues: String*): Node = {
			if (newValues.isEmpty) this
			else insert(newValues.head).insert(newValues.tail : _*)
		}

		def insert(newValue: String): Node = {
			val node = insertR(newValue, false)
			node.color = Black
			node
		}

		private def insertR(newValue: String, sw: Boolean = false): Node = {
			if (value == null) return Node(newValue, Red)

			var node = this

			val is4Node = isRed(node.left) && isRed(node.right)
			if (is4Node) {
				node.color = Red
				node.left.color = Black
				node.right.color = Black
			}
			if (newValue < node.value) {
				node.left = node.left.insertR(newValue, false)
				if (isRed(node) && isRed(node.left) && sw) node = node.rotateRight()
				if (isRed(node.left) && isRed(node.left.left)) {
					node = node.rotateRight()
					node.color = Black
					node.right.color = Red
				}
			} else {
				node.right = node.right.insertR(newValue, true)
				if (isRed(node) && isRed(node.right) && !sw) node = node.rotateLeft()
				if (isRed(node.right) && isRed(node.right.right)) {
					node = node.rotateLeft()
					node.color = Black
					node.left.color = Red
				}
			}
			node
		}

		private def isRed(node: Node): Boolean = {
			if (node == null) false else node.color == Red
		}

		private def rotateRight(): Node = {
			val newRoot = left
			val temp = newRoot.right
			newRoot.right = this
			this.left = temp
			newRoot
		}

		private def rotateLeft(): Node = {
			val newRoot = right
			val temp = newRoot.left
			newRoot.left = this
			this.right = temp
			newRoot
		}

		override def toString = {
			def childAsString(node: Node) = {
				if (node == emptyNode()) "-" else node.toString
			}
			def childrenAsString() = {
				if (left == null && right == null) ""
				else if (left == null && right != null) "," + childAsString(left)
				else "," + childAsString(left) + "," + childAsString(right)
			}
			"Node(" + value + "," + color + childrenAsString() + ")"
		}
	}
}