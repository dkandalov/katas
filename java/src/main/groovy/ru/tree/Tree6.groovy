package ru.tree

import org.junit.Test

/**
 * User: dima
 * Date: 2/4/11
 */
class Tree6 {

  @Test public void shouldTraverseTreePreOrder() {
    assert traversePreOrder(node(1)) == [1]
    assert traversePreOrder(node(1,
        node(11,
            node(111),
            null
        ),
        node(22,
            null,
            node(222)
        )
    )) == [1, 11, 111, 22, 222]
  }

  def traversePreOrder(ANode node) {
    def result = []
    def queue = new LinkedList()
    queue.addLast(node)

    while (!queue.empty) {
      def aNode = queue.removeLast()
      result << aNode.value
      if (aNode.right != null) queue.addLast(aNode.right)
      if (aNode.left != null) queue.addLast(aNode.left)
    }

    result
  }

  def traversePreOrder_r(ANode node) {
    if (node == null) return []

    def result = []
    result << node.value
    result << traversePreOrder(node.left)
    result << traversePreOrder(node.right)
    result.flatten()
  }

  private static def node(value, left = null, right = null) {
    new ANode(value, left, right)
  }
}

final class ANode {
  def value
  def left, right

  ANode(value, left, right) {
    this.value = value
    this.left = left
    this.right = right
  }

  boolean equals(o) {
    if (this.is(o)) return true;
    if (getClass() != o.class) return false;

    ANode aNode = (ANode) o;

    if (left != aNode.left) return false;
    if (right != aNode.right) return false;
    if (value != aNode.value) return false;

    return true;
  }

  int hashCode() {
    int result;
    result = (value != null ? value.hashCode() : 0);
    result = 31 * result + (left != null ? left.hashCode() : 0);
    result = 31 * result + (right != null ? right.hashCode() : 0);
    return result;
  }

  public String toString() {
    return "node{" +
        "value=" + value + '}';
  }
}
