package ru.bsearchtree

import org.scalatest.Matchers
import org.junit.Test


class RedBlackTree extends Matchers {
	private object Color extends Enumeration {
		type Color = Value
		val None, Red, Black = Value
	}
	import Color._


	@Test def `inserting elements as described in 'Algorithms in Java' Figure 13.13`() {
		emptyNode().insert("a") should equal(Node("a", Black))
		emptyNode().insert("a", "s") should equal(Node("a", Black, emptyNode(), Node("s", Red)))
		emptyNode().insert("a", "s", "e") should equal(Node("e", Red, Node("a", Red), Node("s", Red)))
	}


	private def emptyNode(): Node = {
		Node(null, None, null, null)
	}

	private case class Node(value: String, var color: Color = None, var left: Node = emptyNode(), var right: Node = emptyNode()) {
		def insert(newValue: String): Node = {
			val node = insertR(newValue, false)
			node.color = Black
			node
		}

		def insert(newValues: String*): Node = {
			if (newValues.isEmpty) this
			else insert(newValues.head).insert(newValues.tail : _*)
		}

		def insertR(newValue: String, sw: Boolean = false): Node = {
			if (value == null) return Node(newValue, Red)

			var node = this
			if (node.left.color == Red && node.right.color == Red) {
				node.color = Red
				node.left.color = Black
				node.right.color = Black
			}
			if (newValue < value) {
				node.left = node.left.insertR(newValue, false)
				if (node.color == Red && node.left.color == Red && sw) node = node.rotateRight()
				if (node.left.color == Red && node.left.left.color == Red) {
					node = node.rotateRight()
					node.color = Black
					node.right.color = Red
				}
			} else {
				node.right = node.right.insertR(newValue, true)
				if (node.color == Red && node.right.color == Red && !sw) node = node.rotateLeft()
				if (node.right.color == Red && node.right.right.color == Red) {
					node = node.rotateLeft()
					node.color = Black
					node.left.color = Red
				}
			}
			node
		}

		def rotateRight(): Node = {
			val leftRight = left.right
			left.right = this
			this.left = leftRight
			left
		}

		def rotateLeft(): Node = {
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