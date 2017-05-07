package katas.scala.tree

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 26/10/2011
 */
class Tree7 extends Matchers {
  @Test def shouldCalculateTreeProperties () {
    val tree = Node("root",
      Node("1",
        Node("11"),
        Node("12")
      ),
      Node("2",
        Node("21"),
        Node("22")
      )
    )
    val a = { (l:List[String], s:String) => l ::: List(s) }
    traverseInOrder(tree, a, List()) should equal(List("11", "1", "12", "root", "21", "2", "22"))
    traversePreOrder(tree, a, List()) should equal(List("root", "1", "11", "12", "2", "21", "22"))
    traversePostOrder(tree, a, List()) should equal(List("11", "12", "1", "21", "22", "2", "root"))

    heightOf(tree) should equal(2)
    level(1, tree, List(), a) should equal(List("1", "2"))
    level(2, tree, List(), a) should equal(List("11", "12", "21", "22"))

    pathLengthOf(tree) should equal(10)
    internalPathLengthOf(tree) should equal(2)
    externalPathLengthOf(tree) should equal(8)
  }

  def externalPathLengthOf[T](node: Node[T], h: Int = 0): Int = node match {
    case Node(value, null, null) => h
    case Node(value, left, right) => externalPathLengthOf(left, h + 1) + externalPathLengthOf(right, h + 1)
  }

  def internalPathLengthOf[T](node: Node[T], h: Int = 0): Int = node match {
    case Node(value, null, null) => 0
    case Node(value, left, right) => h + internalPathLengthOf(left, h + 1) + internalPathLengthOf(right, h + 1)
  }

  def pathLengthOf[T](node: Node[T], h: Int = 0): Int = node match {
    case null => 0
    case Node(_, left, right) =>
      h + pathLengthOf(left, h + 1) + pathLengthOf(right, h + 1)
  }

  def heightOf[T](node: Node[T]): Int = node match {
    case null => -1
    case Node(_, left, right) => math.max(heightOf(left), heightOf(right)) + 1
  }

  def level[T, U](l: Int, node: Node[T], acc: U, f: Function2[U, T, U], h: Int = 0): U = node match {
    case null => acc
    case Node(value, left, right) =>
      if (l < h)
        acc
      else if (l > h)
        level(l, right, level(l, left, acc, f, h + 1), f, h + 1)
      else
        f(acc, value)
  }

  def traversePreOrder[T, U](node: Node[T], f: Function2[U, T, U], acc: U): U = node match {
    case null => acc
    case Node(value, left, right) =>
      traversePreOrder(right, f, traversePreOrder(left, f, f(acc, value)))
  }

  def traverseInOrder[T, U](node: Node[T], f: Function2[U, T, U], acc: U): U = {
    if (node == null) return acc
    traverseInOrder(node.right, f, f(traverseInOrder(node.left, f, acc), node.value))
  }

  def traversePostOrder[T, U](node: Node[T], f: Function2[U, T, U], acc: U): U = {
    if (node == null) return acc
    f(traversePostOrder(node.right, f, traversePostOrder(node.left, f, acc)), node.value)
  }

  case class Node[T](value: T, left: Node[T] = null, right: Node[T] = null)
}