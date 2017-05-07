package katas.scala.tree

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable

/**
 * User: dima
 * Date: 17/04/2012
 */

class Tree9 extends Matchers {
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

  @Test def stackBasedInOrderTraversal() {
    traverseInOrderS(Node(1)) should equal(Seq(1))
    traverseInOrderS(Node(1, Node(2), Node(3))) should equal(Seq(2, 1, 3))
    traverseInOrderS(Node(1, Node(2, Node(21)), Node(3))) should equal(Seq(21, 2, 1, 3))
  }

  case class ProcessLeft(node: Node)
  case class ProcessValue(node: Node)
  case class ProcessRight(node: Node)

  def traverseInOrderS(node: Node): Seq[Int] = {
    var result = Seq[Int]()
    val stack = mutable.Stack[Any](ProcessLeft(node))

    while (!stack.isEmpty) {
      stack.pop() match {
        case ProcessLeft(n) =>
          stack.push(ProcessValue(n))
          if (n.left != null) stack.push(ProcessLeft(n.left))
        case ProcessValue(n) =>
          result = result :+ n.value
          if (n.right != null) stack.push(ProcessLeft(n.right))
      }
    }
    result
  }

  @Test def stackBasedPostOrderTraversal() {
    traversePostOrderS(Node(1)) should equal(Seq(1))
    traversePostOrderS(Node(1, Node(2), Node(3))) should equal(Seq(2, 3, 1))
    traversePostOrderS(Node(1, Node(2, Node(21)), Node(3))) should equal(Seq(21, 2, 3, 1))
  }

  def traversePostOrderS(node: Node): Seq[Int] = {
    var result = Seq[Int]()
    val stack = mutable.Stack[Any](ProcessLeft(node))

    while (!stack.isEmpty) {
      stack.pop() match {
        case ProcessLeft(n) =>
          stack.push(ProcessRight(n))
          if (n.left != null) stack.push(ProcessLeft(n.left))
        case ProcessRight(n) =>
          stack.push(ProcessValue(n))
          if (n.right != null) stack.push(ProcessLeft(n.right))
        case ProcessValue(n) =>
          result = result :+ n.value
      }
    }
    result
  }



  @Test def severalStacksBasedInOrderTraversal() {
    traverseInOrderSs(Node(1)) should equal(Seq(1))
    traverseInOrderSs(Node(1, Node(2), Node(3))) should equal(Seq(2, 1, 3))
    traverseInOrderSs(Node(1, Node(2, Node(21)), Node(3))) should equal(Seq(21, 2, 1, 3))
    traverseInOrderSs(Node(1, Node(2, Node(21)), Node(3, null, Node(32)))) should equal(Seq(21, 2, 1, 3, 32))
  }

  def traverseInOrderSs(node: Node): Seq[Int] = {
    var result = Seq[Int]()
    val values = mutable.Stack[Int]()
    val lefts = mutable.Stack[Node]()
    val rights = mutable.Stack[Node]()

    def add(node: Node) {
      values.push(node.value)
      lefts.push(node.left)
      rights.push(node.right)
    }

    add(node)

    while (!values.isEmpty || !lefts.isEmpty || !rights.isEmpty) {
      val left = if (lefts.isEmpty) null else lefts.pop()
      if (left != null) {
        add(left)
      } else {
        result = result :+ values.pop()
        val right = if (rights.isEmpty) null else rights.pop()
        if (right != null) add(right)
      }
    }
    result
  }

}