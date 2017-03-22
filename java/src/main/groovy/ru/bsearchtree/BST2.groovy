package ru.bsearchtree

import org.junit.Test
import static ru.sort.bubble.BubbleSort0.shuffledList

/**
 * User: dima
 * Date: 27/3/11
 */
class BST2 {
  @Test public void shouldKeepBSTInvariants_WhenInsertNewNodes() {
    assert node(1) == node(1)
    assert node(1).insert(node(1)) == node(1, null, node(1))
    assert node(1).insert(node(2)) == node(1, null, node(2))
    assert node(1).insert(node(2)).insert(node(0)) == node(1, node(0), node(2))

    def tree = node(2,
        node(0),
        node(4)
    )
    assert tree.insert(node(1)) == node(2,
        node(0, null, node(1)),
        node(4)
    )
  }

  @Test public void shouldSortListsByPreOrderTraversal() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 1]) == [1, 1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]
    assert sort([2, 3, 1]) == [1, 2, 3]

    assert sort(["z", "f", "a"]) == ["a", "f", "z"]

    (100).times {
      assert sort(shuffledList(1..100)) == (1..100).asList()
    }
  }

  static def sort(List list) {
    if (list.size() <= 1) return list

    def firstValue = list[0]
    def tree = list[1..list.size() - 1].inject(node(firstValue)) { Node tree, value ->
      tree.insert(node(value))
    }
    tree.traversePreOrder()
  }

  static Node node(def value, Node left = null, Node right = null) {
    new Node(value, left, right)
  }

  private static class Node {
    def value
    Node left, right

    Node(def value, Node left, Node right) {
      this.value = value
      this.left = left
      this.right = right
    }

    // did over-complex thing several times instead..
    // did something like sink(), swim() for heap structure.. think!
    // (but it was a good indication that I was not able to think on the weekend)
    Node insert(Node node) {
      if (node.value >= value) {
        right == null ? right = node : right.insert(node)
      } else if (node.value < value) {
        left == null ? left = node : left.insert(node)
      }
      this
    }

    List traversePreOrder() { // this is in-order!!
      def result = []
      if (left != null) result << left.traversePreOrder() // forgot to add values to result
      result << value
      if (right != null) result << right.traversePreOrder() // forgot to add values to result
      result.flatten()
    }

    @Override boolean equals(o) {
      if (this.is(o)) return true;
      if (getClass() != o.class) return false;

      Node node = (Node) o;

      if (value != node.value) return false;
      if (left != node.left) return false;
      if (right != node.right) return false;

      return true;
    }

    @Override int hashCode() {
      int result;
      result = value;
      result = 31 * result + (left != null ? left.hashCode() : 0);
      result = 31 * result + (right != null ? right.hashCode() : 0);
      return result;
    }

    @Override public String toString() {
      return "Node{" +
          "value=" + value +
          "${left ? " " + left : ""}" +
          "${right ? " " + right : ""}" +
          '}';
    }
  }
}
