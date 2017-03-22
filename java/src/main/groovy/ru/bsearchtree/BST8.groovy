package ru.bsearchtree

import org.junit.Test

/**
 * User: dima
 * Date: 09/02/2012
 */
class BST8 {
  @Test void GIVEN_aSetOfKeysAndValues_SHOULD_addThemToBST_AND_retrieveValuesByKeys() {
    assert new BST([:]).get("a") == null
    new BST([a: 1]).with {
      assert get("a") == 1
      assert get("b") == null
    }
    new BST([b: 2, a: 1, c: 3]).with {
      assert get("a") == 1
      assert get("b") == 2
      assert get("c") == 3
      assert get("d") == null
    }
    mapPermutations([a: 1, b: 2, c: 3, d: 4, e: 5]).each { Map map ->
      new BST(map).with {
        assert get("1") == null
        assert get("a") == 1
        assert get("b") == 2
        assert get("c") == 3
        assert get("d") == 4
        assert get("e") == 5
        assert get("f") == null
      }
    }
  }

  @Test void GIVEN_BST_SHOULD_removeValuesByKeys() { // tOdO add more tests
    assert new BST([:]).remove("a") == null
    new BST([a: 1]).with {
      assert remove("b") == null
      assert remove("a") == 1
    }
    new BST([b: 2, a: 1, c: 3]).with {
      assert remove("d") == null
      assert remove("a") == 1
      assert remove("b") == 2
      assert remove("c") == 3
    }
  }

  static mapPermutations(Map map) {
    [a: 1, b: 2, c: 3, d: 4, e: 5].entrySet().toList().permutations().collect { listOfEntries ->
      listOfEntries.inject([:]) { acc, entry -> acc.put(entry.key, entry.value); acc }
    }
  }

  private static class BST {
    def root

    BST(Map map) {
      map.each{ put_(it.key, it.value) }
    }

    // non-recursive
    def put_(key, value) {
      if (root == null) {
        root = new Node(key, value)
        return
      }

      Node node = root
      while (true) {
        if (key > node.key) {
          if (node.right == null) {
            node.right = new Node(key, value)
            return
          } else {
            node = node.right
          }
        } else {
          if (node.left == null) {
            node.left = new Node(key, value)
            return
          } else {
            node = node.left
          }
        }
      }
    }

    def put(key, value) {
      root = doPut(key, value, root)
    }

    private doPut(key, value, Node node) {
      if (node == null) new Node(key, value)
      else if (key > node.key) node.withRight(doPut(key, value, node.right))
      else node.withLeft(doPut(key, value, node.left))
    }

    def get(key) {
      doGet(key, root)
    }

    def remove(key) {
      def result = doRemove(key, root)
      root = result.node
      result.value
    }

    private doRemove(key, Node node) {
      if (node == null) {
        [value: null, node: null]
      } else if (key == node.key) {
        [value: node.value, node: join(node.left, node.right)]
      } else if (key < node.key) {
        def result = doRemove(key, node.left)
        [value: result.value, node: node.withLeft(result.node)]
      } else if (key > node.key) {
        def result = doRemove(key, node.right)
        [value: result.value, node: node.withRight(result.node)]
      }
    }

    private join(Node left, Node right) {
      if (left == null) right
      else if (right == null) left
      else smallestToRoot(right).withLeft(left)
    }

    private Node smallestToRoot(Node node) {
      if (node == null) null
      else if (node.left == null) node
      else rotateRight(node.withLeft(smallestToRoot(node)))
    }

    private rotateRight(Node node) {
      def newRoot = node.left
      newRoot.withRight(node.withLeft(newRoot.right))
    }

    private doGet(key, Node node) {
      if (node == null) null
      else if (key == node.key) node.value
      else if (key > node.key) doGet(key, node.right)
      else doGet(key, node.left)
    }
  }

  private static class Node {
    def key
    def value
    Node left
    Node right

    Node(key, value, Node left = null, Node right = null) {
      this.key = key
      this.value = value
      this.left = left
      this.right = right
    }

    def withRight(node) { new Node(key, value, left, node) }

    def withLeft(node) { new Node(key, value, node, right) }
  }
}
