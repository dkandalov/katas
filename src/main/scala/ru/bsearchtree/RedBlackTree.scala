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
		emptyNode().insert("a") should equal(Node("a", Red))
		emptyNode().insert("a", "s") should equal(Node("a", Red, emptyNode(), Node("s", Red)))
		emptyNode().insert("a", "s", "e") should equal(Node("e", Red, Node("a", Red), Node("s", Red)))
	}


	private def emptyNode(): Node = {
		Node(null, None, null, null)
	}

	private case class Node(value: String, color: Color = None, var left: Node = emptyNode(), var right: Node = emptyNode()) {
		def insert(newValue: String, sw: Boolean = false): Node = {
			if (value == null) return Node(newValue, Red)

			if (newValue < value) {
				left = left.insert(newValue)
//				if (color == Red && left.color == Red && )
			} else {
				right = right.insert(newValue)
			}
			this
		}

		def insert(newValues: String*): Node = {
			if (newValues.isEmpty) this
			else insert(newValues.head).insert(newValues.tail : _*)
		}
	}
}