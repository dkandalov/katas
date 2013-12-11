package ru._99_problems

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test


class P7 extends ShouldMatchers {
	@Test def `P70C (*) Count the nodes of a multiway tree.`() {
		MTree('a').nodeCount should equal(1)
		MTree('a', MTree('b')).nodeCount should equal(2)
		MTree('a', MTree('b'), MTree('c')).nodeCount should equal(3)
	}

	case class MTree[+T](value: T, children: List[MTree[T]]) {
		def nodeCount: Int = 1 + children.map(_.nodeCount).sum
		override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
	}

	object MTree {
		def apply[T](value: T, children: MTree[T]*) = new MTree(value, children.toList)
	}

}