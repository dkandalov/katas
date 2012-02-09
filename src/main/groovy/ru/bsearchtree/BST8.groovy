package ru.bsearchtree

import org.junit.Test

/**
 * User: dima
 * Date: 09/02/2012
 */
class BST8 {
  @Test public void GIVEN_aSetOfKeysAndValues_SHOULD_addThemToBST_AND_retrieveValuesByKeys() {
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

  static mapPermutations(Map map) {
    [a: 1, b: 2, c: 3, d: 4, e: 5].entrySet().toList().permutations().collect { listOfEntries ->
      listOfEntries.inject([:]) { acc, entry -> acc.put(entry.key, entry.value); acc }
    }
  }

  private static class BST {
    def root

    BST(Map map) {
      map.each{ put(it.key, it.value) }
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
