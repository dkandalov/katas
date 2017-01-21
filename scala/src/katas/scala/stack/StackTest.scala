package katas.scala.stack

import org.junit.Test
import org.scalatest.Matchers


class StackTest extends Matchers {
	@Test def `push and pop elements from stack`() {
		var stack = new Stack[Int]()
		stack.size should equal(0)

		stack = stack.push(1).push(2).push(3)
		stack.size should equal(3)

		val (value, updatedStack) = stack.pop
		updatedStack.size should equal(2)
		value should equal(3)
	}

	@Test def `covariance`() {
		var anyStack = new Stack[AnyVal]()
		val intStack = new Stack[Int]()

		val s1: Stack[AnyVal] = anyStack.push("abc")
		val s2: Stack[AnyVal] = anyStack.push(123)
		val s3: Stack[Any] = intStack.push("abc")
		val s4: Stack[Int] = intStack.push(123)

		anyStack = intStack
	}
}