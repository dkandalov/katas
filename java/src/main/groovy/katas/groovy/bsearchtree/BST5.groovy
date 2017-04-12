package katas.groovy.bsearchtree

import groovy.transform.Immutable
import org.junit.Ignore
import org.junit.Test
import static katas.groovy.bsearchtree.BST5.Node.*

/**
 * User: dima
 * Date: 22/11/2011
 */
class BST5 {
  @Test void should_add_elements_keeping_its_structure() {
    assert emptyNode() == emptyNode()
    assert emptyNode().add(1) == node(1)

    assert node(1).add(2) == node(1, null, node(2))
    assert node(1).add(0) == node(1, node(0))

    assert node(1).add(0).add(2) == node(1, node(0), node(2))
    assert node(1).add(2).add(0) == node(1, node(0), node(2))
  }

  @Test void should_determine_if_it_contains_an_element() {
    assert emptyNode().contains(0) == false

    def tree = node(1, node(0), node(2))
    assert tree.contains(0)
    assert [-1, 0, 1, 2, 3].collect { tree.contains(it) } == [false, true, true, true, false]
  }

  @Test public void should_perform_right_rotation_of_node() {
    assert emptyNode().rotateRight() == emptyNode()

    assert node(1).rotateRight() == node(1)
    assert node(1, null, node(2)).rotateRight() == node(1, null, node(2))
    assert node(1, node(0), null).rotateRight() == node(0, null, node(1))
    assert node(1, node(0), node(2)).rotateRight() == node(0, null, node(1, null, node(2)))

    assert node(3,
            node(1, node(0), node(2)),
            node(5, node(4), node(6)),
    ).rotateRight() == node(1,
            node(0),
            node(3, node(2), node(5, node(4), node(6)))
    )
  }

  @Test void should_perform_left_rotation_of_node() {
    assert emptyNode().rotateLeft() == emptyNode()

    assert node(1).rotateLeft() == node(1)
    assert node(1, null, node(2)).rotateLeft() == node(2, node(1))
    assert node(1, node(0), null).rotateLeft() == node(1, node(0))
    assert node(1, node(0), node(2)).rotateLeft() == node(2, node(1, node(0)))

    assert node(3,
            node(1, node(0), node(2)),
            node(5, node(4), node(6)),
    ).rotateLeft() == node(5,
            node(3, node(1, node(0), node(2)), node(4)),
            node(6)
    )
  }

  @Ignore
  @Test void should_find_the_smallest_element() {
    assert emptyNode().findSmallest() == emptyNode()
  }

  @Test void should_move_the_smallest_element_to_root() {
    assert moveSmallestToRoot(node(1)) == node(1)
    assert moveSmallestToRoot(node(1, node(0))) == node(0, null, node(1))
    assert moveSmallestToRoot(node(1, node(0), node(2))) == node(0, null, node(1, null, node(2)))
    assert moveSmallestToRoot(node(3, node(1, node(0), node(2)))) == node(0)
  }

  @Test void should_remove_elements_keeping_structure() {
    assert emptyNode().remove(1) == emptyNode()
    assert node(1).remove(1) == emptyNode()
    assert node(1, node(0), node(2)).remove(0) == node(1, null, node(2))
    assert node(1, node(0), node(2)).remove(2) == node(1, node(0))
    assert node(1, node(0), node(2)).remove(1) == node(2, node(0))

    assert node(1, node(0), node(2)).remove(1) == node(2, node(0)) // TODO
  }

  @Immutable
  static class Node {
    static Node EMPTY_NODE = new Node(Integer.MIN_VALUE, null, null)

    int value
    Node left
    Node right

    static Node node(value, left = null, right = null) {
      new Node(value, left, right)
    }

    static Node emptyNode() {
      EMPTY_NODE
    }

    def contains(valueToFind) {
      if (this == EMPTY_NODE) {
        false
      } else if (valueToFind == value) {
        true
      } else if (valueToFind < value) {
        left != null ? left.contains(valueToFind) : false
      } else { //if (valueToFind > value) {
        right != null ? right.contains(valueToFind) : false
      }
    }

    Node add(int value) {
      if (this == EMPTY_NODE) node(value)
      else addTo(this, node(value))
    }

    Node rotateRight() {
      if (this == EMPTY_NODE || left == null) this
      else node(left.value, left.left, node(value, left.right, right))
    }

    Node rotateLeft() {
      if (this == EMPTY_NODE || right == null) this
      else node(right.value, node(value, left, right.left), right.right)
    }

    Node remove(int valueToRemove) {
      if (this == EMPTY_NODE) {
        this
      } else {
        def result = removeFrom(this, valueToRemove)
        result == null ? EMPTY_NODE : result
      }
    }

    static Node removeFrom(Node aNode, int valueToRemove) {
      if (valueToRemove == aNode.value) {
        removeRootOf(aNode)
      } else if (valueToRemove < aNode.value) {
        aNode.left == null ? aNode : node(aNode.value, removeFrom(aNode.left, valueToRemove), aNode.right)
      } else {
        aNode.right == null ? aNode : node(aNode.value, aNode.left, removeFrom(aNode.right, valueToRemove))
      }
    }

    Node removeRoot() {
      if (this == EMPTY_NODE) this
      else if (right == null && left == null) EMPTY_NODE
      else removeRootOf(this)
    }

    static Node removeRootOf(Node aNode) {
      if (aNode.right == null) aNode.left
      else {
        def newRoot = moveSmallestToRoot(aNode.right)
        node(newRoot.value, aNode.left, newRoot.right)
      }
    }

    static Node moveSmallestToRoot(Node node) {
      if (node.left == null && node.right == null) node
      else node.rotateRight()
    }

    private static Node addTo(Node thisNode, Node nodeToAdd) {
      if (thisNode == null) {
        nodeToAdd
      } else if (nodeToAdd.value >= thisNode.value) {
        node(thisNode.value, thisNode.left, addTo(thisNode.right, nodeToAdd))
      } else { //if (nodeToAdd.value < thisNode.value) {
        node(thisNode.value, addTo(thisNode.left, nodeToAdd), thisNode.right)
      }
    }

    @Override
    String toString() {
      "Node{${value}"+ ", left=" + left + ", right=" + right + '}';
    }
  }

}
