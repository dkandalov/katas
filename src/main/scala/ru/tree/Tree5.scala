package ru.tree

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/*
 * User: dima
 * Date: 19/2/11
 * Time: 8:04 AM
 */
class Tree5 extends AssertionsForJUnit {
  @Test def shouldTraverseTreeInDifferentOrder() {
    val tree = createTree()
    assert(traversePreOrder(tree) === List(0, 1, 11, 12, 2, 21, 22))
    assert(traverseInOrder(tree) === List(1, 11, 12, 0, 2, 21, 22))
    assert(traversePostOrder(tree) === List(1, 11, 12, 2, 21, 22, 0))
  }

  @Test def shouldTraverseNodesWithinDepthLevels() {
    val bigTree =
      new Node(0,
        new Node(1,
          new Node(11,
            new Node(111),
            new Node(112) // copy-pasted extra comma
          ),
          new Node(12,
            new Node(121),
            new Node(122)
          )
        ),
        new Node(2,
          new Node(21,
            new Node(211), // forgot to add EmptyNode, EmptyNode.. then added constructor for Node
            new Node(212) // copy-pasted extra comma
          ),
          new Node(22,
            new Node(221),
            new Node(222)
          )
        )
      )

    assert(traversePreOrder(bigTree, 1, 2) === List(1, 11, 12, 2, 21, 22))
  }

  def traversePreOrder(node: BinTree, minDepth: Int = 0, maxDepth: Int = Int.MaxValue, depth: Int = 0): List[Int] = {
    node match {
      case EmptyNode => List()
      case Node(value, left, right) => // used depth comparison without considering that recursive calls should be continued after "depth < minDepth"
        val aValue = if (depth >= minDepth && depth <= maxDepth) List(value) else List() // used "value" and "List()" as return types
        aValue :::
          traversePreOrder(left, minDepth, maxDepth, depth + 1) :::
          traversePreOrder(right, minDepth, maxDepth, depth + 1) // forgot to rename this method after copy-paste
    }
  }

  def traversePreOrder(node: BinTree): List[Int] = node match {
    case EmptyNode => List()
    case Node(value, left, right) => value :: traversePreOrder(left) ::: traversePreOrder(right) // used :: instead of ::: to prepend list with list
  }

  def traverseInOrder(node: BinTree): List[Int] = node match {
    case EmptyNode => List()
    case Node(value, left, right) => traversePreOrder(left) ::: List(value) ::: traversePreOrder(right)
  }

  def traversePostOrder(node: BinTree): List[Int] = node match {
    case EmptyNode => List()
    case Node(value, left, right) => traversePreOrder(left) ::: traversePreOrder(right) ::: List(value)
  }

  def createTree(): BinTree = {
    new Node(0,
      new Node(1,
        new Node(11, EmptyNode, EmptyNode),
        new Node(12, EmptyNode, EmptyNode)
      ),
      new Node(2,
        new Node(21, EmptyNode, EmptyNode),
        new Node(22, EmptyNode, EmptyNode)
      )
    )
  }

  abstract class BinTree

  case object EmptyNode extends BinTree

  case class Node(value: Int, left: BinTree, right: BinTree) extends BinTree {
    def this(value: Int) = this (value, EmptyNode, EmptyNode)
  }

}
