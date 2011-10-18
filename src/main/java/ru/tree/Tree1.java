package ru.tree;

import java.util.LinkedList;
import java.util.Queue;

/**
 * User: dima
 * Date: Aug 26, 2010
 */
public class Tree1 {
    public static void main(String[] args) {
        Node root = new Node("1",
                new Node("11"),
                new Node("12")
        );

        printTree(root);
    }

    private static void printTree(Node node) {
        Queue<Node> deque = new LinkedList<Node>();
        deque.add(node);

        while (!deque.isEmpty()) {
            node = deque.remove();
            if (node == null) continue;

            System.out.println(node.value);
            deque.add(node.left);
            deque.add(node.right);
        }
    }

    private static void r_printTree(Node node) {
        if (node == null) return;
        System.out.println(node.value);
        r_printTree(node.left);
        r_printTree(node.right);
    }

    private static class Node {
        Node left;
        Node right;
        String value;

        public Node(String value) {
            this(value, null, null);
        }

        private Node(String value, Node left, Node right) {
            this.left = left;
            this.right = right;
            this.value = value;
        }
    }
}
