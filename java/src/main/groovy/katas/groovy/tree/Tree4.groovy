package katas.groovy.tree

import org.junit.Test

/**
 * User: dima
 * Date: 18/2/11
 */
class Tree4 {
    @Test public void shouldTraverseTree() {
        Node tree = createTree()
        assert traversePreOrder(tree) == [0, 1, 11, 12, 2, 21, 22]
        assert traverseInOrder(tree) == [11, 1, 12, 0, 21, 2, 22]
        assert traversePostOrder(tree) == [11, 12, 1, 21, 22, 2, 0]

        assert traversePreOrder_(tree) == [0, 1, 11, 12, 2, 21, 22]
        assert traverseInOrder_(tree) == [11, 1, 12, 0, 21, 2, 22]
        assert traversePostOrder_(tree) == [11, 12, 1, 21, 22, 2, 0]
        assert traverseLevelOrder_(tree) == [0, 1, 2, 11, 12, 21, 22]
    }

    def traverseLevelOrder_(Node node) {
        def result = []
        def queue = new LinkedList() << node

        while (!queue.empty) {
            node = (Node) queue.removeFirst()
            if (node == null) continue
            result << node.value
            queue << node.left << node.right // didn't check order left/right after copying code
        }
        result
    }

    def traversePostOrder_(def node) { // didn't manage to make this method work for 2 pomodoros.. then introduced Value class and did it in 5 min
        def result = []
        def queue = new LinkedList() << node

        while (!queue.empty) {
            node = queue.removeLast()
            if (node == null) continue

            if (node instanceof Value) {
                result << node.node.value
            } else {
                queue << new Value(node) << node.right << node.left
            }
        }
        result
    }

    def traverseInOrder_(def node) {
        def result = []
        def queue = new LinkedList() << node

        while (!queue.empty) {
            node = queue.removeLast()
            if (node == null) continue

            if (node instanceof Value) {
                result << node.node.value
            } else {
                queue << node.right << new Value(node) << node.left
            }
        }
        result
    }

    def traversePreOrder_(Node node) {
        def result = []
        def queue = new LinkedList() << node

        while (!queue.empty) {
            node = (Node) queue.removeLast()
            if (node == null) continue
            result << node.value
            queue << node.right << node.left
        }
        result
    }

    def traversePreOrder(Node node) {
        if (node == null) return []
        [node.value] + traversePreOrder(node.left) + traversePreOrder(node.right)
    }

    def traverseInOrder(Node node) {
        if (node == null) return []
        traverseInOrder(node.left) + [node.value] + traverseInOrder(node.right) // copy-pasted as traversePreOrder
    }

    def traversePostOrder(Node node) {
        if (node == null) return []
        traversePostOrder(node.left) + traversePostOrder(node.right) + [node.value] // copy-pasted as traversePreOrder
    }

    def createTree() {
        new Node(0,
                new Node(1,
                        new Node(11),
                        new Node(12)
                ),
                new Node(2,
                        new Node(21),
                        new Node(22)
                )
        )
    }

    private static final class Value {
        def node

        Value(node) {
            this.node = node
        }
    }

    private static final class Node {
        def value
        Node left
        Node right

        Node(value) {
            this(value, null, null)
        }

        Node(value, Node left, Node right) {
            this.value = value
            this.left = left
            this.right = right
        }
    }
}
