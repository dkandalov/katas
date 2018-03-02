package katas.scala.bsearchtree

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/*
 * User: dima
 * Date: 23/2/11
 * Time: 5:41 AM
 */
class BST1 extends AssertionsForJUnit {

  @Test def shouldKeepBinaryTreeSorted() {
    var tree: ANode = new Node(2)
    assert(traverseTreeInOrder(tree) === List(2))

    tree = add(tree, 0) // forgot to reassign "tree"
    assert(traverseTreeInOrder(tree) === List(0, 2))
    tree = add(tree, 4)
    assert(traverseTreeInOrder(tree) === List(0, 2, 4))
    tree = add(tree, 1)
    assert(traverseTreeInOrder(tree) === List(0, 1, 2, 4))
    tree = add(tree, 3)
    assert(traverseTreeInOrder(tree) === List(0, 1, 2, 3, 4))
  }

  @Test def shouldTraverseTreeInOrder() {
    val tree = Node(2,
	    new Node(1), // used "Node" instead of "new Node"
	    Node(3,
		    EmptyNode,
		    new Node(4)) // forgot emptynode arguments
    )
    assert(traverseTreeInOrder(tree) === List(1, 2, 3, 4))
  }

  def add(node: ANode, newValue: Int): ANode = node match {
    case EmptyNode => new Node(newValue)
    case Node(value, left, right) =>
      if (value > newValue)
        Node(value, add(left, newValue), right)
      else
        Node(value, left, add(right, newValue))
  }

  def traverseTreeInOrder(node: ANode): List[Int] = node match {
    case EmptyNode => List()
    case Node(value, left, right) => traverseTreeInOrder(left) ::: List(value) ::: traverseTreeInOrder(right)
  }

  abstract class ANode

  case object EmptyNode extends ANode // used "class" instead of "object".. couldn't compile for few minutes

  case class Node(value: Int, left: ANode, right: ANode) extends ANode {
    def this(value: Int) = this (value, EmptyNode, EmptyNode)
  }

}
