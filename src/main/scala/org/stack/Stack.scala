package org

import org.junit.Test
import org.scalatest.Matchers


class Stack[T] {
	def push(value: T): Stack[T] = {
		???
	}

	def pop: (T, Stack[T]) = {
		???
	}
}

class StackTest extends Matchers {
	@Test def ``(): Unit = {
		1 should equal(2)
	}
}