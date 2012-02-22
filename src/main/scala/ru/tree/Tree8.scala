package ru.tree

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import ru.util.Pomodoro

/**
 * User: dima
 * Date: 21/02/2012
 */
@Pomodoro("1")
class Tree8 extends ShouldMatchers {
  @Test def inOrderTraversal() {
    traverseInOrder(Node(1)) should equal(Seq(1))
    traverseInOrder(Node(1, Node(0))) should equal(Seq(0, 1))
    traverseInOrder(Node(1, Node(0), Node(2))) should equal(Seq(0, 1, 2))
    traverseInOrder(Node(1, Node(0), Node(2, Node(3), Node(5)))) should equal(Seq(0, 1, 3, 2, 5))
  }

  @Test def postOrderTraversal() {
    traversePostOrder(Node(1)) should equal(Seq(1))
    traversePostOrder(Node(1, Node(0))) should equal(Seq(0, 1))
    traversePostOrder(Node(1, Node(0), Node(2))) should equal(Seq(0, 2, 1))
    traversePostOrder(Node(1, Node(0), Node(2, Node(3), Node(5)))) should equal(Seq(0, 3, 5, 2, 1))
  }

  @Test def preOrderTraversal() {
    traversePreOrder(Node(1)) should equal(Seq(1))
    traversePreOrder(Node(1, Node(0))) should equal(Seq(1, 0))
    traversePreOrder(Node(1, Node(0), Node(2))) should equal(Seq(1, 0, 2))
    traversePreOrder(Node(1, Node(0), Node(2, Node(3), Node(5)))) should equal(Seq(1, 0, 2, 3, 5))
  }

  @Test def shouldCutTreeByDepth() {
    cutTree(Node(1), 1) should equal(Node(1))
    cutTree(Node(1, Node(0)), 1) should equal(Node(1))
    cutTree(Node(1, Node(0), Node(2, Node(3), Node(5))), 1) should equal(Node(1))
    cutTree(Node(1, Node(0), Node(2, Node(3), Node(5))), 2) should equal(Node(1, Node(0), Node(2)))
  }

  def cutTree[T](node: Node[T], depth: Int): Node[T] = node match {
    case Node(_, null, null) => if (depth == 1) node else null
    case Node(v, left, right) =>
      if (depth == 1) Node(v)
      else Node(v, cutTree(node.left, depth - 1), cutTree(node.right, depth - 1))
  }

  def traversePreOrder[T](node: Node[T]): Seq[T] = node match {
    case null => Seq()
    case _ => Seq(node.value) ++ traversePreOrder(node.left) ++ traversePreOrder(node.right)
  }

  def traversePostOrder[T](node: Node[T]): Seq[T] = node match {
    case null => Seq()
    case _ => traversePostOrder(node.left) ++ traversePostOrder(node.right) ++ Seq(node.value)
  }

  def traverseInOrder[T](node: Node[T]): Seq[T] = node match {
    case null => Seq()
    case _ => traverseInOrder(node.left) ++ Seq(node.value) ++ traverseInOrder(node.right)
  }

  case class Node[T](value:T, left: Node[T] = null, right: Node[T] = null)
}