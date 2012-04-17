package ru.tree

import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import collection.mutable

/**
 * User: dima
 * Date: 17/04/2012
 */

class Tree9 extends ShouldMatchers {
  case class Node(value: Int, left: Node = null, right: Node = null)

  @Test def inOrderTraversal() {
    traverseInOrder(Node(1)) should equal(Seq(1))
    traverseInOrder(Node(1, Node(2), Node(3))) should equal(Seq(2, 1, 3))
    traverseInOrder(Node(1, Node(2, Node(21)), Node(3))) should equal(Seq(21, 2, 1, 3))
  }

  def traverseInOrder(node: Node): Seq[Int] = {
    if (node == null) Seq()
    else traverseInOrder(node.left).:+(node.value) ++ traverseInOrder(node.right)
  }

  @Test def preOrderTraversal() {
    traversePreOrder(Node(1)) should equal(Seq(1))
    traversePreOrder(Node(1, Node(2), Node(3))) should equal(Seq(1, 2, 3))
    traversePreOrder(Node(1, Node(2, Node(21)), Node(3))) should equal(Seq(1, 2, 21, 3))
  }

  def traversePreOrder(node: Node): Seq[Int] = {
    if (node == null) Seq()
    else Seq(node.value) ++ traversePreOrder(node.left) ++ traversePreOrder(node.right)
  }

  @Test def postOrderTraversal() {
    traversePostOrder(Node(1)) should equal(Seq(1))
    traversePostOrder(Node(1, Node(2), Node(3))) should equal(Seq(2, 3, 1))
    traversePostOrder(Node(1, Node(2, Node(21)), Node(3))) should equal(Seq(21, 2, 3, 1))
  }

  def traversePostOrder(node: Node): Seq[Int] = {
    if (node == null) Seq()
    else traversePostOrder(node.left) ++ traversePostOrder(node.right) :+ node.value
  }


  @Test def breadthFirstTraversal() {
    traverseBreadthFirst(Node(1)) should equal(Seq(1))
    traverseBreadthFirst(Node(1, Node(2), Node(3))) should equal(Seq(1, 2, 3))
    traverseBreadthFirst(Node(1, Node(2, Node(21)), Node(3))) should equal(Seq(1, 2, 3, 21))
  }

  def traverseBreadthFirst(node: Node): Seq[Int] = {
    var result = Seq[Int]()
    val queue = mutable.Queue(node)

    while (!queue.isEmpty) {
      val n = queue.dequeue()
      if (n != null) {
        queue.enqueue(n.left, n.right)
        result = result :+ n.value
      }
    }
    result
  }


  @Test def stackBasedPreOrderTraversal() {
    traversePreOrderS(Node(1)) should equal(Seq(1))
    traversePreOrderS(Node(1, Node(2), Node(3))) should equal(Seq(1, 2, 3))
    traversePreOrderS(Node(1, Node(2, Node(21)), Node(3))) should equal(Seq(1, 2, 21, 3))
  }

  def traversePreOrderS(node: Node): Seq[Int] = {
    var result = Seq[Int]()
    val stack = mutable.Stack(node)

    while (!stack.isEmpty) {
      val n = stack.pop()
      if (n != null) {
        stack.push(n.right, n.left)
        result = result :+ n.value
      }
    }
    result
  }

  @Test def queueBasedInOrderTraversal() {
    // TODO
  }
}