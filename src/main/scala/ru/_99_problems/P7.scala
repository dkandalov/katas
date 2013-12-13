package ru._99_problems

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test


class P7 extends ShouldMatchers {
	@Test def `P70C (*) Count the nodes of a multiway tree.`() {
		MTree('a').nodeCount should equal(1)
		MTree('a', MTree('b')).nodeCount should equal(2)
		MTree('a', MTree('b'), MTree('c')).nodeCount should equal(3)
	}

	@Test def `P70 (**) Tree construction from a node string.`() {
		MTree('a').asString should equal("a^")
		MTree('a', MTree('b'), MTree('c')).asString should equal("ab^c^^")
		MTree('a', MTree('f', MTree('g')), MTree('c'), MTree('b', MTree('d'), MTree('e'))).asString should equal("afg^^c^bd^e^^^")

		MTree.string2MTree("ab^c^^") should equal(MTree('a', MTree('b'), MTree('c')))
		MTree.string2MTree("afg^^c^bd^e^^^") should equal(MTree('a', MTree('f', MTree('g')), MTree('c'), MTree('b', MTree('d'), MTree('e'))))
	}

	@Test def `P71 (*) Determine the internal path length of a tree.`() {
		"a^".internalPathLength should equal(0)
		"ab^^".internalPathLength should equal(1)
		"abc^^^".internalPathLength should equal(3)
		"afg^^c^bd^e^^^".internalPathLength should equal(9)
	}

	@Test def `P72 (*) Construct the postorder sequence of the tree nodes.`() {
		"a^".postorder should equal(List('a'))
		"abc^^^".postorder should equal(List('c', 'b', 'a'))
		"afg^^c^bd^e^^^".postorder should equal(List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
	}

	implicit def stringToMTree(s: String): MTree[Char] = MTree.string2MTree(s)

	case class MTree[+T](value: T, children: List[MTree[T]]) {
		def postorder: List[T] = {
			children.flatMap(_.postorder) ::: List(value)
		}

		def internalPathLength: Int = {
			children.map(_.nodeCount).sum + children.map(_.internalPathLength).sum
		}

		def nodeCount: Int = 1 + children.map(_.nodeCount).sum

		override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

		def asString: String = value + children.map(_.asString).mkString("") + "^"
	}

	object MTree {
		def apply[T](value: T, children: MTree[T]*) = new MTree(value, children.toList)

		def string2MTree(string: String): MTree[Char] = {
			def consumeMTreeFrom(s: String): (MTree[Char], String) = {
				val value = s.head
				var children = List[MTree[Char]]()

				var tail = s.tail
				while (tail.head != '^') {
					val (child, newTail) = consumeMTreeFrom(tail)
					tail = newTail
					children = children :+ child
				}
				tail = tail.tail // drop '^'

				(MTree(value, children), tail)
			}
			consumeMTreeFrom(string)._1
		}
	}
}