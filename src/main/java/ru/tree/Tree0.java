package ru.tree;

import org.junit.Test;

import java.util.Deque;
import java.util.LinkedList;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: DKandalov
 */
public class Tree0
{
    private static Node createTree()
    {
        return node("root",
                node("1",
                        node("11"),
                        node("12")),
                node("2",
                        node("21"),
                        null)
        );
    }

    @Test
    public void shouldVisitTreeNodes()
    {
        Node root = createTree();

        assertThat(preOrderTraverse(root), equalTo("root-1-11-12-2-21-"));
        assertThat(postOrderTraverse(root), equalTo("root-2-21-1-12-11-"));
        assertThat(inOrderTraverse(root), equalTo("root-1-11-12-2-21-"));
    }

    private static String inOrderTraverse(Node root)
    {
        Deque<Node> deque = new LinkedList<Node>();
        deque.addFirst(root);

        String s = "";
        while (!deque.isEmpty()) {
            Node node = deque.removeFirst();
            if (node == null) continue;

            s += node.value + "-";
            deque.addFirst(node.right);
            deque.addFirst(node.left);
        }
        return s;
    }

    private static String preOrderTraverse(Node node)
    {
        if (node == null) return "";

        String s = "";
        s += node.value + "-";
        s += preOrderTraverse(node.left);
        s += preOrderTraverse(node.right);
        return s;
    }

    private static String postOrderTraverse(Node node)
    {
        if (node == null) return "";

        String s = "";
        s += node.value + "-";
        s += postOrderTraverse(node.right);
        s += postOrderTraverse(node.left);
        return s;
    }

    private static Node node(String value)
    {
        return node(value, null, null);
    }

    private static Node node(String value, Node left, Node right)
    {
        return new Node(value, left, right);
    }

    private static class Node
    {
        String value;
        Node left;
        Node right;

        private Node(String value, Node left, Node right)
        {
            this.value = value;
            this.left = left;
            this.right = right;
        }
    }
}
