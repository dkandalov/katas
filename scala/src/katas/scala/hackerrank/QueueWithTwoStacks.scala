package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

/**
	* https://www.hackerrank.com/challenges/queue-using-two-stacks
	*/
class QueueWithTwoStacks extends Matchers {
	
	@Test def `example from the task`(): Unit = {
		val queue = new Queue()

		queue.push(42)
		queue.toList shouldEqual List(42)

		queue.pop() shouldEqual 42

		queue.push(14)
		queue.peek shouldEqual 14

		queue.push(28)
		queue.peek shouldEqual 14
		queue.toList shouldEqual List(14, 28)

		queue.push(60)
		queue.push(78)
		queue.toList shouldEqual List(14, 28, 60, 78)

		queue.pop() shouldEqual 14
		queue.pop() shouldEqual 28
		queue.toList shouldEqual List(60, 78)
	}

	@Test def `performance for large queue`(): Unit = {
		val queue = new Queue()
		0.until(100000).foreach(queue.push)
		0.until(100000).foreach { i =>
			queue.pop() shouldEqual i
		}
	}

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)
		val amountOfQueries = scanner.nextInt()

		val queue = new Queue()
		0.until(amountOfQueries).foreach { _ =>
			val queryType = scanner.nextInt()
			if (queryType == 1) {
				queue.push(scanner.nextInt())
			} else if (queryType == 2) {
				queue.pop()
			} else if (queryType == 3) {
				println(queue.peek)
			}
		}
	}

	class Queue {
		private val s1 = scala.collection.mutable.Stack[Int]()
		private val s2 = scala.collection.mutable.Stack[Int]()

		def push(elem: Int): Unit = {
			s1.push(elem)
		}

		def pop(): Int = {
			if (s2.isEmpty) {
				while (s1.nonEmpty) s2.push(s1.pop)
			}
			s2.pop()
		}

		def peek: Int = {
			if (s2.isEmpty) {
				while (s1.nonEmpty) s2.push(s1.pop)
			}
			s2.head
		}

		def toList: List[Int] = {
			s2.elems ++ s1.elems.reverse
		}
	}
}