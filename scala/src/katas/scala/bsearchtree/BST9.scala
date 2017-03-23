package katas.scala.bsearchtree

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 15/02/2012
 */
class BST9 extends Matchers {
  @Test def WHEN_insertingValueToBST_it_SHOULD_beInsertedAtTheRoot() {
    BST().add(List(1)) should equal(BST(Node(1)))
    BST().add(List(1, 2)) should equal(BST(Node(2, Node(1), null)))
    BST().add(List(2, 1)) should equal(BST(Node(1, null, Node(2))))
    BST().add(List(1, 2, 3)) should equal(BST(Node(3,Node(2,Node(1))))) // was wrong test case
    BST().add(List(3, 2, 1)) should equal(BST(Node(1, null, Node(2, null, Node(3)))))
    BST().add(List(1, 3, 2)) should equal(BST(Node(2, Node(1), Node(3)))) // was wrong test case
  }

  case class BST(root: Node = null) {

    def add(list: List[Int]): BST = {
      BST(add(list, root))
    }

    def add(list: List[Int], node: Node): Node = list match {
      case List() => node
      case x :: xs => add(xs, add(x, node))
    }

    def add(key: Int, node: Node): Node = {
      if (node == null) Node(key)
      else if (key < node.key) rotateRight(node.withLeft(add(key, node.left)))
      else rotateLeft(node.withRight(add(key, node.right)))
    }

    def rotateRight(node: Node): Node = {
      val newRoot = node.left
      val newRight = node.withLeft(newRoot.right)
      newRoot.withRight(newRight)
    }

    def rotateLeft(node: Node): Node = {
      val newRoot = node.right
      val newLeft = node.withRight(newRoot.left) // had "newRoot" instead of "newRoot.left"
      newRoot.withLeft(newLeft)
    }
  }

  case class Node(key: Int, left: Node = null, right: Node = null) {
    def withLeft(node: Node) = Node(key, node, right)
    def withRight(node: Node) = Node(key, left, node)
  }
}