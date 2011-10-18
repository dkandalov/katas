package ru.bsearchtree

import org.junit.Test

/**
 * User: dima
 * Date: 22/2/11
 */
class BST0 {
    @Test public void shouldKeepTreeSorted() {
        def tree = node(3)
        tree.add(1)
        assert (tree ==
                node(3,
                        node(1),
                        null))
        assert traverseInOrder(tree) == [1, 3]
        assert !tree.contains(0)
        assert tree.contains(1)
        assert !tree.contains(2)
        assert tree.contains(3)
        assert !tree.contains(4)

        tree.add(4)
        assert tree ==
                node(3,
                        node(1),
                        node(4))
        assert traverseInOrder(tree) == [1, 3, 4]
        assert !tree.contains(0)
        assert tree.contains(1)
        assert !tree.contains(2)
        assert tree.contains(3)
        assert tree.contains(4)

        tree.add(2)
        assert tree ==
                node(3,
                        node(1,
                                null,
                                node(2)),
                        node(4))
        assert traverseInOrder(tree) == [1, 2, 3, 4]

        tree =
            node(2,
                    node(0),
                    node(4)
            )
        assert traverseInOrder(tree) == [0, 2, 4]
        tree.add(1)
        assert traverseInOrder(tree) == [0, 1, 2, 4]

    }

    static def node(value) { new Node(value, null, null) }

    static def node(value, left, right) { new Node(value, left, right) }

    static List traverseInOrder(Node node) {
        if (node == null) return []
        traverseInOrder(node.left) + [node.value] + traverseInOrder(node.right)
    }

}

private static final class Node {
    int value
    Node left
    Node right

    Node(int value) {
        this(value, null, null)
    }

    Node(int value, Node left, Node right) {
        this.value = value
        this.left = left
        this.right = right
    }

    Node add(value) {
        doAdd(this, value)
    }

    private Node doAdd(Node node, def value) {
        if (node == null) {
            return new Node(value, null, null)
        }

        if (value < node.value) {
            node.left = doAdd(node.left, value)
        } else {
            node.right = doAdd(node.right, value)
        }
        return node // forgot to return node after recursive calls
    }

    boolean contains(int value) {
        doesContain(this, value)
    }

    private boolean doesContain(Node node, int value) {
        if (node == null) return false

        if (value == node.value) {
            true
        } else if (value < node.value) {
            doesContain(node.left, value)
        } else {
            doesContain(node.right, value)
        }
    }

    boolean equals(o) {
        if (this.is(o)) return true;
        if (getClass() != o.class) return false;

        Node node = (Node) o;

        if (value != node.value) return false;
        if (left != node.left) return false;
        if (right != node.right) return false;

        return true;
    }

    int hashCode() {
        int result;
        result = value;
        result = 31 * result + (left != null ? left.hashCode() : 0);
        result = 31 * result + (right != null ? right.hashCode() : 0);
        return result;
    }

    public String toString() {
        return "Node{" +
                "value=" + value +
                ", left=" + left +
                ", right=" + right +
                '}';
    }
}