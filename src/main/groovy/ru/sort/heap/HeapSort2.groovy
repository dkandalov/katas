package ru.sort.heap

import org.junit.Test

/**
 * User: dima
 * Date: 25/2/11
 */
class HeapSort2 {
    @Test public void shouldSortListUsingHeapSort() {
        assert sort([]) == []
        assert sort([1]) == [1]
        assert sort([2, 1]) == [1, 2]
        assert sort([2, 3, 1]) == [1, 2, 3]
        assert sort([2, 3, 4, 1]) == [1, 2, 3, 4]

        [2, 3, 4, 5, 6].each { i ->
            (1..i).toList().permutations().each {
                assert sort(it) == (1..i).asList()
            }
        }
    }

    @Test public void performance() {
        GUtil.measure(100, 1000) {
            [6].each { i ->
                (1..i).toList().permutations().each {
                    assert sort(it) == (1..i).asList()
                }
            }
        }.print()
    }

    @Test public void heapShouldPreserveItsStructureAfterInsertion() {
        def heap = new Heap()
        assert heap.root == null

        heap << 1
        assert heap.root == node(1)

        heap << 2
        assert heap.root == node(1, node(2), null) // used wrong expectation (min element should in the root)

        heap << 0
        assert heap.root ==
                node(0,
                        node(1,
                                node(2),
                                null),
                        null)
    }

    def sort(List list) {
        def heap = new Heap()
        list.each { heap << it }

        def result = []
        while (!heap.isEmpty()) {
            result << heap.take()
        }
        result
    }

    private static class Heap {
        Node root

        def leftShift(int value) {
            def oldRoot = root
            root = new Node(value, oldRoot, null) // used new Node(value, oldRoot?.left, oldRoot?.right) what is stupid

            sink(root)
        }

        private def sink(Node node) {
            if (node.left == null && node.right == null) return
            Node maxChild = maxNode(node.left, node.right)

            if (node.value > maxChild.value) {
                exchangeValues(node, maxChild)
                if (maxChild.value == Integer.MAX_VALUE && maxChild.left == null && maxChild.right == null) {
                    if (node.left == maxChild)
                        node.left = null
                    else if (node.right == maxChild)
                        node.right = null
                    return
                }
                sink(maxChild)
            }
        }

        private def maxNode(Node node1, Node node2) {
            if (node1 != null && node2 == null) return node1
            if (node2 != null && node1 == null) return node2

            node1.value > node2.value ? node1.value : node2.value
        }

        private def exchangeValues(Node node1, Node node2) {
            int tmp = node1.value
            node1.value = node2.value
            node2.value = tmp
        }

        int take() { // didn't think about taking values before implementing Heap
            def result = root.value

            root.value = Integer.MAX_VALUE // used MIN_VALUE instead of MAX_VALUE
            sink(root)
            if (root.value == Integer.MAX_VALUE) root = null // used root == Integer.MIN_VALUE instead of "root.value"

            result
        }

        boolean isEmpty() {
            root == null
        }
    }

    private static Node node(int value) {
        node(value, null, null)
    }

    private static Node node(int value, left, right) {
        new Node(value, left, right)
    }

    private static class Node {
        def value
        Node left
        Node right

        Node(value, Node left, Node right) {
            this.value = value
            this.left = left
            this.right = right
        }

        public String toString() {
            return "Node{" +
                    "value=" + value +
                    (left != null ? ", left=${left}" : "") +
                    (right != null ? ", right=${right}" : "") +
                    '}';
        }

        boolean equals(o) { // forgot to implement equals/hashcode before running junit test
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
    }

}
