package ru.tree

import org.junit.Test

/**
 * User: DKandalov
 */
class Tree2 {
  @Test
  public void shouldTraverseTreeDepthFirst() {
    def tree =
    node(0,
            node(1,
                    node(11),
                    node(12)
            ),
            node(2,
                    node(21),
                    node(22),
            )
    )
    assert traverseInOrder(tree) == [11, 1, 12, 0, 21, 2, 22]
    assert traversePreOrder(tree) == [0, 1, 11, 12, 2, 21, 22]
    assert traversePostOrder(tree) == [11, 12, 1, 21, 22, 2, 0]

    assert traversePreOrder_nr(tree) == [0, 1, 11, 12, 2, 21, 22]
    assert traverse(tree) == [0, 1, 2, 11, 12, 21, 22]
  }

  static def traverse(Node node) {
    def result = []
    def queue = [node]

    while (!queue.empty) {
      node = queue.first()
      queue.remove(0)
      if (node == null) continue

      result << node.value
      queue << node.left << node.right
    }
    result
  }

  static def traversePreOrder_nr(Node node) {
    def result = []
    def queue = [node]

    while (!queue.empty) {
      node = queue.pop()
      if (node == null) continue

      result << node.value
      queue << node.right << node.left
    }
    result
  }

  static def traverseInOrder(Node node) {
    if (node == null) return []
    [] + traverseInOrder(node.left) + [node.value] + traverseInOrder(node.right)
  }

  static def traversePreOrder(Node node) {
    if (node == null) return []
    [] + [node.value] + traversePreOrder(node.left) + traversePreOrder(node.right)
  }

  static def traversePostOrder(Node node) {
    if (node == null) return []
    [] + traversePostOrder(node.left) + traversePostOrder(node.right) + [node.value]
  }

  def node(def value, def left = null, def right = null) {
    new Node(value, left, right)
  }

  private static class Node {
    def value
    def left
    def right

    Node(value, left, right) {
      this.value = value
      this.left = left
      this.right = right
    }

    boolean equals(o) {
      if (this.is(o)) return true;
      if (getClass() != o.class) return false;

      Node node = (Node) o;

      if (left != node.left) return false;
      if (right != node.right) return false;
      if (value != node.value) return false;

      return true;
    }

    int hashCode() {
      int result;
      result = (value != null ? value.hashCode() : 0);
      result = 31 * result + (left != null ? left.hashCode() : 0);
      result = 31 * result + (right != null ? right.hashCode() : 0);
      return result;
    }

    String toString() {
      return "Node{" +
              "value=" + value +
              ", left=" + left +
              ", right=" + right +
              '}';
    }
  }
}
