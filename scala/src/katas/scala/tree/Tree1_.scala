package katas.scala.tree

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 * User: DKandalov
 */

class Tree1_ extends AssertionsForJUnit {
  @Test def shouldTraverseTreeInOrder() {
    assert(traverseInOrder(null) === List())
    assert(traverseInOrder(Node(0)) === List(0))
    assert(traverseInOrder(Node(0, Node(1), Node(2))) === List(1, 0, 2))

    val tree: Node = Node(0, Node(1, Node(11), Node(12)), Node(2))
    assert(traverseInOrder(tree) === List(11, 1, 12, 0, 2))
  }

  def traverseInOrder(node: Node): List[Int] = node match {
    case null => List()
    case Node(value, left, right) => traverseInOrder(left) ::: List(value) ::: traverseInOrder(right)
  }
}

case class Node(value: Int, left: Node = null, right: Node = null)